package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.ResolveState;
import consulo.typescript.language.psi.TypeScriptTypes;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
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
    public static final String DEFAULT_EXPORT = "default";

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
        for (PsiElement statement : getTopLevelStatements(file)) {
            IElementType type = statement.getNode().getElementType();
            if (type == TypeScriptTypes.IMPORT_DECLARATION || type == TypeScriptTypes.EXPORT_DECLARATION
                || type == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT) {
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
     * Resolves a module specifier: a path relative to the importing file ({@code './x'}, with the TypeScript and
     * JavaScript extensions and {@code index} files), or a package in {@code node_modules} ({@code types} of its
     * package.json, {@code index.d.ts}, {@code @types/pkg}).
     */
    @Nullable
    @RequiredReadAction
    public static PsiFile resolveModule(PsiElement context, String specifier) {
        PsiFile containingFile = context.getContainingFile().getOriginalFile();
        VirtualFile file = containingFile.getVirtualFile();
        VirtualFile directory = file != null ? file.getParent() : null;
        if (directory == null) {
            return null;
        }
        VirtualFile resolved = specifier.startsWith(".") || specifier.startsWith("/")
            ? findModuleFile(directory, specifier)
            : findPackage(directory, specifier);
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
     * The declarations a module exports under a name ({@link #DEFAULT_EXPORT} for {@code export default}).
     */
    @RequiredReadAction
    public static List<PsiElement> findExports(PsiFile module, String name) {
        List<PsiElement> result = new ArrayList<>();
        collectExports(module, name, result, new HashSet<>());
        return result;
    }

    private static void collectExports(PsiFile module, String name, List<PsiElement> result, Set<PsiFile> visited) {
        if (!visited.add(module)) {
            return;
        }
        for (PsiElement statement : getTopLevelStatements(module)) {
            if (statement.getNode().getElementType() == TypeScriptTypes.EXPORT_DEFAULT_ASSIGNMENT) {
                if (DEFAULT_EXPORT.equals(name)) {
                    for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
                        if (child instanceof JSElement || child instanceof TypeScriptElementImpl) {
                            result.add(child);
                            break;
                        }
                    }
                }
                continue;
            }
            if (statement.getNode().getElementType() != TypeScriptTypes.EXPORT_DECLARATION) {
                continue;
            }
            ASTNode node = statement.getNode();
            boolean isDefault = node.findChildByType(JSTokenTypes.DEFAULT_KEYWORD) != null;
            ASTNode namedExports = node.findChildByType(TypeScriptTypes.NAMED_EXPORTS);
            String specifier = getModuleSpecifier(statement);

            if (namedExports != null) {
                for (ASTNode exportSpecifier : namedExports.getChildren(null)) {
                    if (exportSpecifier.getElementType() != TypeScriptTypes.EXPORT_SPECIFIER) {
                        continue;
                    }
                    ASTNode[] identifiers = exportSpecifier.getChildren(TokenSet.create(JSTokenTypes.IDENTIFIER));
                    if (identifiers.length == 0 || !name.equals(identifiers[identifiers.length - 1].getText())) {
                        continue;
                    }
                    String localName = identifiers[0].getText();
                    if (specifier != null) {
                        PsiFile target = resolveModule(statement, specifier);
                        if (target != null) {
                            collectExports(target, localName, result, visited);
                        }
                    }
                    else {
                        result.addAll(findTopLevelDeclarations(module, localName));
                    }
                }
            }
            else if (specifier != null) {
                // export * from './x' (not the default export)
                if (!DEFAULT_EXPORT.equals(name) && node.findChildByType(TypeScriptTypes.MULT) != null) {
                    PsiFile target = resolveModule(statement, specifier);
                    if (target != null) {
                        collectExports(target, name, result, visited);
                    }
                }
            }
            else if (isDefault) {
                if (DEFAULT_EXPORT.equals(name)) {
                    for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
                        if (child instanceof JSElement || child instanceof TypeScriptElementImpl) {
                            result.add(child);
                            break;
                        }
                    }
                }
            }
            else {
                ResolveProcessor processor = new ResolveProcessor(name, module);
                statement.processDeclarations(processor, ResolveState.initial(), null, module);
                PsiElement found = processor.getResult();
                if (found != null) {
                    result.add(found);
                }
            }
        }
    }

    /**
     * The top-level declarations of a file with a name (exported or not).
     */
    @RequiredReadAction
    public static List<PsiElement> findTopLevelDeclarations(PsiFile file, String name) {
        List<PsiElement> result = new ArrayList<>();
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
     * Everything a module exports under a name (completion of {@code ns.}); re-exports are not followed.
     */
    @RequiredReadAction
    public static List<PsiElement> findAllExports(PsiFile module) {
        List<PsiElement> result = new ArrayList<>();
        ResolveProcessor processor = new ResolveProcessor(null, module);
        for (PsiElement statement : getTopLevelStatements(module)) {
            if (statement.getNode().getElementType() == TypeScriptTypes.EXPORT_DECLARATION
                && statement.getNode().findChildByType(JSTokenTypes.DEFAULT_KEYWORD) == null) {
                statement.processDeclarations(processor, ResolveState.initial(), null, module);
            }
        }
        result.addAll(processor.getResults());
        return result;
    }
}
