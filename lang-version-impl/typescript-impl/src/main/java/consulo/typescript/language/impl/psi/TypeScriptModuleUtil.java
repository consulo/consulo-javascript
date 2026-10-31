package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSClass;
import consulo.javascript.module.resolve.JSModuleExport;
import consulo.javascript.module.resolve.JavaScriptModuleExports;
import java.util.HashMap;
import java.util.Map;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.javascript.module.resolve.JavaScriptModuleResolution;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.impl.psi.PsiFileImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.StubElement;
import consulo.typescript.language.impl.stub.TypeScriptFileStub;
import consulo.typescript.language.psi.TypeScriptTypes;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * ES modules of TypeScript: module files, module specifiers ({@code './x'}, {@code 'pkg'}) and their exports.
 *
 * @since 2026-10-30
 */
public final class TypeScriptModuleUtil {
    public static final String DEFAULT_EXPORT = JavaScriptModuleExports.DEFAULT_EXPORT;

    /**
     * The name in another module of {@code import * as ns} / {@code export * as ns}: the module itself.
     */
    public static final String NAMESPACE_EXPORT = JSModuleExport.NAMESPACE_EXPORT;

    private static final String[] EXTENSIONS = {".ts", ".tsx", ".d.ts", ".js", ".jsx", ".mjs", ".cjs"};
    private static final Pattern TYPES_FIELD = Pattern.compile("\"(?:types|typings)\"\\s*:\\s*\"([^\"]+)\"");

    private TypeScriptModuleUtil() {
    }

    /**
     * The top-level statements of a file, without the generic {@code STATEMENT} wrapper.
     */
    @RequiredReadAction
    public static List<PsiElement> getTopLevelStatements(PsiFile file) {
        List<PsiElement> result = new ArrayList<>();
        for (PsiElement child = file.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getNode().getElementType() == TypeScriptTypes.STATEMENT) {
                for (PsiElement inner = child.getFirstChild(); inner != null; inner = inner.getNextSibling()) {
                    if (inner instanceof JSElement || inner instanceof TypeScriptElementImpl) {
                        result.add(inner);
                    }
                }
            }
            else if (child instanceof JSElement || child instanceof TypeScriptElementImpl) {
                result.add(child);
            }
        }
        return result;
    }

    /**
     * A file with a top-level {@code import} or {@code export} is a module: its declarations aren't global.
     */
    @RequiredReadAction
    public static boolean isModule(PsiFile file) {
        // the stub: the global resolve asks it for every candidate file, the AST of lib.dom.d.ts isn't loaded for that
        if (file instanceof PsiFileImpl fileImpl) {
            StubElement<?> stub = fileImpl.getGreenStub();
            if (stub == null) {
                stub = fileImpl.getStub();
            }
            if (stub instanceof TypeScriptFileStub typeScriptStub) {
                return typeScriptStub.isModule();
            }
        }
        return isModuleByTree(file);
    }

    /**
     * Whether a declaration is global: a declaration of a script, or of {@code declare global { ... }} of a module (the
     * names of the file stub - Nuxt declares its auto-imports so). Not a declaration a module keeps to itself.
     */
    @RequiredReadAction
    public static boolean isGlobalDeclaration(PsiElement declaration) {
        PsiFile file = declaration.getContainingFile();
        if (file == null) {
            return false;
        }
        if (!isModule(file)) {
            return true;
        }
        String name = declaration instanceof PsiNamedElement named ? named.getName() : null;
        return name != null && getGlobalNames(file).contains(name);
    }

    /**
     * The names declared by {@code declare global { ... }} of a module, by its stub.
     */
    @RequiredReadAction
    public static Set<String> getGlobalNames(PsiFile file) {
        if (file instanceof PsiFileImpl fileImpl) {
            StubElement<?> stub = fileImpl.getGreenStub();
            if (stub == null) {
                stub = fileImpl.getStub();
            }
            if (stub instanceof TypeScriptFileStub typeScriptStub) {
                return typeScriptStub.getGlobalNames();
            }
        }
        return getGlobalNamesByTree(file);
    }

    /**
     * The names declared by {@code declare global { ... }} blocks of a file, from its AST (building the stub).
     */
    @RequiredReadAction
    public static Set<String> getGlobalNamesByTree(PsiFile file) {
        Set<String> result = new LinkedHashSet<>();
        for (PsiElement statement : getTopLevelStatements(file)) {
            ASTNode node = statement.getNode();
            if (node.getElementType() != TypeScriptTypes.AMBIENT_DECLARATION || node.findChildByType(TypeScriptTypes.GLOBAL_KEYWORD) == null) {
                continue;
            }
            ASTNode block = node.findChildByType(TypeScriptTypes.BLOCK_STATEMENT);
            for (PsiElement child = block == null ? null : block.getPsi().getFirstChild(); child != null; child = child.getNextSibling()) {
                collectDeclaredNames(child, result);
            }
        }
        return Set.copyOf(result);
    }

    private static void collectDeclaredNames(PsiElement element, Set<String> result) {
        IElementType type = element.getNode().getElementType();
        if (type == TypeScriptTypes.STATEMENT || type == TypeScriptTypes.EXPORT_DECLARATION || type == TypeScriptTypes.AMBIENT_DECLARATION) {
            for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                collectDeclaredNames(child, result);
            }
        }
        else if (element instanceof JSVarStatement varStatement) {
            for (JSVariable variable : varStatement.getVariables()) {
                if (variable.getName() != null) {
                    result.add(variable.getName());
                }
            }
        }
        else if (element instanceof PsiNamedElement named && named.getName() != null) {
            result.add(named.getName());
        }
    }

    /**
     * Whether a file has an import or export at the top level, from its AST (building the stub).
     */
    public static boolean isModuleByTree(PsiFile file) {
        for (PsiElement statement : getTopLevelStatements(file)) {
            IElementType type = statement.getNode().getElementType();
            if (type == TypeScriptTypes.IMPORT_DECLARATION || type == TypeScriptTypes.EXPORT_DECLARATION
                || type == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT || isDecoratedExport(statement)) {
                return true;
            }
        }
        return false;
    }

    /**
     * The module specifier of an {@code import} / {@code export ... from}, without quotes.
     */
    @Nullable
    @RequiredReadAction
    public static String getModuleSpecifier(PsiElement importOrExport) {
        for (ASTNode child = importOrExport.getNode().getFirstChildNode(); child != null; child = child.getTreeNext()) {
            IElementType type = child.getElementType();
            if (type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL) {
                String text = child.getText();
                return text.length() >= 2 ? text.substring(1, text.length() - 1) : null;
            }
        }
        return null;
    }

    /**
     * Resolves a module specifier by the module resolvers (relative paths: the relative resolver of base-impl; npm
     * packages), else a package in {@code node_modules} ({@code types} of its package.json, {@code index.d.ts},
     * {@code @types/pkg}) - without the Node.js plugin.
     */
    @Nullable
    @RequiredReadAction
    public static PsiFile resolveModule(PsiElement context, String specifier) {
        // the module resolvers (relative paths; npm packages: exports conditions, @types, workspace packages), then the
        // node_modules packages of TypeScript itself
        PsiFile byResolver = JavaScriptModuleResolution.resolve(context, specifier, JavaScriptModuleRequest.Kind.TYPES);
        if (byResolver != null) {
            return byResolver;
        }
        PsiFile containingFile = context.getContainingFile().getOriginalFile();
        VirtualFile file = containingFile.getVirtualFile();
        VirtualFile directory = file != null ? file.getParent() : null;
        if (directory == null || specifier.startsWith(".") || specifier.startsWith("/")) {
            return null;
        }
        VirtualFile resolved = findPackage(directory, specifier);
        return resolved != null ? PsiManager.getInstance(context.getProject()).findFile(resolved) : null;
    }

    @Nullable
    private static VirtualFile findModuleFile(VirtualFile directory, String path) {
        // import './x.js' also means x.ts
        String withoutJs = path.endsWith(".js") ? path.substring(0, path.length() - 3) : path;
        for (String candidate : new String[]{withoutJs, path}) {
            for (String extension : EXTENSIONS) {
                VirtualFile file = directory.findFileByRelativePath(candidate + extension);
                if (file != null && !file.isDirectory()) {
                    return file;
                }
            }
        }
        VirtualFile exact = directory.findFileByRelativePath(path);
        if (exact != null && !exact.isDirectory()) {
            return exact;
        }
        if (exact != null) {
            return findPackageEntry(exact);
        }
        return null;
    }

    @Nullable
    private static VirtualFile findPackage(VirtualFile directory, String name) {
        for (VirtualFile current = directory; current != null; current = current.getParent()) {
            VirtualFile nodeModules = current.findChild("node_modules");
            if (nodeModules == null) {
                continue;
            }
            VirtualFile packageDirectory = nodeModules.findFileByRelativePath(name);
            if (packageDirectory != null && packageDirectory.isDirectory()) {
                VirtualFile entry = findPackageEntry(packageDirectory);
                if (entry != null) {
                    return entry;
                }
            }
            VirtualFile declarations = findModuleFile(nodeModules, name);
            if (declarations != null) {
                return declarations;
            }
            String typesName = name.startsWith("@") ? name.substring(1).replace('/', '_') : name;
            VirtualFile typesDirectory = nodeModules.findFileByRelativePath("@types/" + typesName);
            if (typesDirectory != null && typesDirectory.isDirectory()) {
                VirtualFile entry = findPackageEntry(typesDirectory);
                if (entry != null) {
                    return entry;
                }
            }
        }
        return null;
    }

    @Nullable
    private static VirtualFile findPackageEntry(VirtualFile packageDirectory) {
        VirtualFile packageJson = packageDirectory.findChild("package.json");
        if (packageJson != null) {
            try {
                Matcher matcher = TYPES_FIELD.matcher(new String(packageJson.contentsToByteArray(), StandardCharsets.UTF_8));
                if (matcher.find()) {
                    VirtualFile types = packageDirectory.findFileByRelativePath(matcher.group(1));
                    if (types != null && !types.isDirectory()) {
                        return types;
                    }
                }
            }
            catch (IOException ignored) {
            }
        }
        for (String extension : EXTENSIONS) {
            VirtualFile index = packageDirectory.findChild("index" + extension);
            if (index != null) {
                return index;
            }
        }
        return null;
    }

    /**
     * {@code @Component({...}) export class A {}} (or {@code export default}): a class declaration with the
     * {@code export} keyword of its own, after its decorators.
     */
    public static boolean isDecoratedExport(PsiElement statement) {
        ASTNode node = statement.getNode();
        return node.getElementType() == TypeScriptTypes.CLASS_DECLARATION && node.findChildByType(JSTokenTypes.EXPORT_KEYWORD) != null;
    }

    /**
     * The declarations a module exports under a name ({@link #DEFAULT_EXPORT} for {@code export default}), following
     * re-exports into modules of any language.
     */
    @RequiredReadAction
    public static List<PsiElement> findExports(PsiFile module, String name) {
        return JavaScriptModuleExports.findExports(module, name);
    }

    /**
     * The exports of a module: by its stub (the module file isn't parsed when an import resolves), by the AST when it's
     * loaded.
     */
    @RequiredReadAction
    public static List<JSModuleExport> getExports(PsiFile module) {
        TypeScriptFileStub stub = getFileStub(module);
        return stub != null ? stub.getExports() : getExportsByTree(module);
    }

    /**
     * Adds the declarations a TypeScript module exports under a name ({@link TypeScriptModuleExports}).
     */
    @RequiredReadAction
    public static void collectExports(PsiFile module, String name, List<PsiElement> result, Set<JavaScriptModuleExports.Visit> visited) {
        for (JSModuleExport export : getExports(module)) {
            switch (export.kind()) {
                case LOCAL -> {
                    if (name.equals(export.name())) {
                        String localName = export.localName();
                        if (localName != null) {
                            result.addAll(findTopLevelDeclarations(module, localName));
                        }
                        else {
                            addDefaultExpression(module, result);
                        }
                    }
                }
                case REEXPORT -> {
                    String localName = export.localName();
                    PsiFile target = name.equals(export.name()) && localName != null ? resolveModule(module, export.specifier()) : null;
                    if (target != null) {
                        if (NAMESPACE_EXPORT.equals(localName)) {
                            result.add(target);
                        }
                        else {
                            JavaScriptModuleExports.collect(target, localName, result, visited);
                        }
                    }
                }
                case STAR -> {
                    // export * from './x' re-exports the named exports, not the default one
                    PsiFile target = DEFAULT_EXPORT.equals(name) ? null : resolveModule(module, export.specifier());
                    if (target != null) {
                        JavaScriptModuleExports.collect(target, name, result, visited);
                    }
                }
                case EQUALS -> {
                    String localName = export.localName();
                    List<PsiElement> entities = localName == null ? List.of() : findTopLevelDeclarations(module, localName);
                    if (DEFAULT_EXPORT.equals(name)) {
                        result.addAll(entities);
                    }
                    else {
                        // export = express: the members of namespace express are the named exports
                        for (PsiElement entity : entities) {
                            if (entity instanceof JSClass namespace && TypeScriptResolveUtil.isNamespace(namespace)) {
                                result.addAll(TypeScriptResolveUtil.findMembers(namespace, name));
                            }
                        }
                    }
                }
            }
        }
    }

    @Nullable
    private static PsiFile resolveModule(PsiFile module, @Nullable String specifier) {
        return specifier == null ? null : resolveModule((PsiElement) module, specifier);
    }

    /**
     * An anonymous {@code export default <expression>}: the expression, from the AST (rare in declaration files).
     */
    @RequiredReadAction
    private static void addDefaultExpression(PsiFile module, List<PsiElement> result) {
        for (PsiElement statement : getTopLevelStatements(module)) {
            if (statement.getNode().getElementType() == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT) {
                PsiElement value = getDefaultValue(statement);
                if (value != null) {
                    result.add(value);
                }
            }
        }
    }

    @Nullable
    private static PsiElement getDefaultValue(PsiElement exportDefault) {
        for (PsiElement child = exportDefault.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSElement || child instanceof TypeScriptElementImpl) {
                return child;
            }
        }
        return null;
    }

    /**
     * The exports of a module from its AST (building the stub). An imported name exported again is a re-export of the
     * other module, so the stub resolves it without the import.
     */
    @RequiredReadAction
    public static List<JSModuleExport> getExportsByTree(PsiFile file) {
        List<PsiElement> statements = getTopLevelStatements(file);
        // local name -> [the name in the other module, its specifier]
        Map<String, String[]> imports = new HashMap<>();
        for (PsiElement statement : statements) {
            if (statement.getNode().getElementType() == TypeScriptTypes.IMPORT_DECLARATION) {
                collectImports(statement, imports);
            }
        }

        List<JSModuleExport> result = new ArrayList<>();
        for (PsiElement statement : statements) {
            ASTNode node = statement.getNode();
            IElementType type = node.getElementType();
            boolean isDefault = node.findChildByType(JSTokenTypes.DEFAULT_KEYWORD) != null;
            if (isDecoratedExport(statement)) {
                String name = statement instanceof PsiNamedElement named ? named.getName() : null;
                if (isDefault) {
                    result.add(JSModuleExport.local(DEFAULT_EXPORT, name));
                }
                else if (name != null) {
                    result.add(JSModuleExport.local(name, name));
                }
            }
            else if (type == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT) {
                PsiElement value = getDefaultValue(statement);
                String name = value instanceof PsiNamedElement named ? named.getName() : getReferencedName(value);
                result.add(exportOf(DEFAULT_EXPORT, name, imports));
            }
            else if (type == TypeScriptTypes.EXPORT_DECLARATION) {
                collectExportDeclaration(statement, imports, result);
            }
        }
        return List.copyOf(result);
    }

    private static void collectExportDeclaration(PsiElement statement, Map<String, String[]> imports, List<JSModuleExport> result) {
        ASTNode node = statement.getNode();
        String specifier = getModuleSpecifier(statement);
        ASTNode namedExports = node.findChildByType(TypeScriptTypes.NAMED_EXPORTS);
        if (namedExports != null) {
            for (ASTNode exportSpecifier : namedExports.getChildren(TokenSet.create(TypeScriptTypes.EXPORT_SPECIFIER))) {
                ASTNode[] identifiers = exportSpecifier.getChildren(TokenSet.create(JSTokenTypes.IDENTIFIER));
                if (identifiers.length == 0) {
                    continue;
                }
                String localName = identifiers[0].getText();
                String name = identifiers[identifiers.length - 1].getText();
                result.add(specifier != null ? JSModuleExport.reexport(name, localName, specifier) : exportOf(name, localName, imports));
            }
        }
        else if (node.findChildByType(TypeScriptTypes.MULT) != null) {
            if (specifier == null) {
                return;
            }
            // export * as ns from './x'
            ASTNode as = node.findChildByType(TypeScriptTypes.AS_KEYWORD);
            ASTNode alias = as == null ? null : as.getTreeNext();
            while (alias != null && alias.getElementType() != JSTokenTypes.IDENTIFIER) {
                alias = alias.getTreeNext();
            }
            result.add(alias != null ? JSModuleExport.reexport(alias.getText(), NAMESPACE_EXPORT, specifier) : JSModuleExport.star(specifier));
        }
        else if (node.findChildByType(TypeScriptTypes.EQ) != null) {
            ASTNode entity = node.findChildByType(JSTokenTypes.IDENTIFIER);
            if (entity != null) {
                result.add(JSModuleExport.equals(entity.getText()));
            }
        }
        else {
            Set<String> names = new LinkedHashSet<>();
            collectDeclaredNames(statement, names);
            for (String name : names) {
                result.add(JSModuleExport.local(name, name));
            }
        }
    }

    /**
     * An export of a local name: a re-export when the name is imported.
     */
    private static JSModuleExport exportOf(String name, @Nullable String localName, Map<String, String[]> imports) {
        String[] imported = localName == null ? null : imports.get(localName);
        return imported != null ? JSModuleExport.reexport(name, imported[0], imported[1]) : JSModuleExport.local(name, localName);
    }

    private static void collectImports(PsiElement importDeclaration, Map<String, String[]> imports) {
        String specifier = getModuleSpecifier(importDeclaration);
        if (specifier == null) {
            return;
        }
        TokenSet identifiers = TokenSet.create(JSTokenTypes.IDENTIFIER);
        for (ASTNode child = importDeclaration.getNode().getFirstChildNode(); child != null; child = child.getTreeNext()) {
            IElementType type = child.getElementType();
            if (type == TypeScriptTypes.DEFAULT_IMPORT_BINDING || type == TypeScriptTypes.NAMESPACE_IMPORT) {
                ASTNode[] names = child.getChildren(identifiers);
                if (names.length > 0) {
                    String importedName = type == TypeScriptTypes.DEFAULT_IMPORT_BINDING ? DEFAULT_EXPORT : NAMESPACE_EXPORT;
                    imports.put(names[names.length - 1].getText(), new String[]{importedName, specifier});
                }
            }
            else if (type == TypeScriptTypes.NAMED_IMPORTS) {
                for (ASTNode importSpecifier : child.getChildren(TokenSet.create(TypeScriptTypes.IMPORT_SPECIFIER))) {
                    ASTNode[] names = importSpecifier.getChildren(identifiers);
                    if (names.length > 0) {
                        imports.put(names[names.length - 1].getText(), new String[]{names[0].getText(), specifier});
                    }
                }
            }
        }
    }

    /**
     * The name of {@code export default foo}: a plain reference.
     */
    @Nullable
    private static String getReferencedName(@Nullable PsiElement value) {
        if (value == null || value.getNode().getElementType() != TypeScriptTypes.REFERENCE_EXPRESSION) {
            return null;
        }
        ASTNode[] children = value.getNode().getChildren(null);
        return children.length == 1 && children[0].getElementType() == JSTokenTypes.IDENTIFIER ? children[0].getText() : null;
    }

    /**
     * The top-level declarations of a file with a name (exported or not): by the stub of the file, by the AST when it's
     * loaded.
     */
    @RequiredReadAction
    public static List<PsiElement> findTopLevelDeclarations(PsiFile file, String name) {
        List<PsiElement> result = new ArrayList<>();
        StubElement<?> stub = getStub(file);
        if (stub != null) {
            for (StubElement<?> child : stub.getChildrenStubs()) {
                PsiElement element = child.getPsi();
                if (element instanceof JSVarStatement) {
                    for (StubElement<?> variable : child.getChildrenStubs()) {
                        if (variable.getPsi() instanceof JSVariable jsVariable && name.equals(jsVariable.getName())) {
                            result.add(jsVariable);
                        }
                    }
                }
                else if (element instanceof PsiNamedElement named && name.equals(named.getName())) {
                    result.add(element);
                }
            }
            return result;
        }
        ResolveProcessor processor = new ResolveProcessor(name, file);
        for (PsiElement statement : getTopLevelStatements(file)) {
            statement.processDeclarations(processor, ResolveState.initial(), null, file);
        }
        for (ResolveResult resolveResult : processor.getResultsAsResolveResults()) {
            if (resolveResult.getElement() instanceof PsiNamedElement element) {
                result.add(element);
            }
        }
        return result;
    }

    /**
     * Everything a module declares as its own named exports (completion of {@code ns.}); re-exports are not followed.
     */
    @RequiredReadAction
    public static List<PsiElement> findAllExports(PsiFile module) {
        List<PsiElement> result = new ArrayList<>();
        for (JSModuleExport export : getExports(module)) {
            String localName = export.localName();
            if (export.kind() == JSModuleExport.Kind.LOCAL && !DEFAULT_EXPORT.equals(export.name()) && localName != null) {
                result.addAll(findTopLevelDeclarations(module, localName));
            }
        }
        return result;
    }

    @Nullable
    private static TypeScriptFileStub getFileStub(PsiFile file) {
        return getStub(file) instanceof TypeScriptFileStub stub ? stub : null;
    }

    /**
     * The stub of a file whose AST isn't loaded (the green stub, else the stub from the index).
     */
    @Nullable
    private static StubElement<?> getStub(PsiFile file) {
        if (!(file instanceof PsiFileImpl fileImpl)) {
            return null;
        }
        StubElement<?> stub = fileImpl.getGreenStub();
        return stub != null ? stub : fileImpl.getStub();
    }
}
