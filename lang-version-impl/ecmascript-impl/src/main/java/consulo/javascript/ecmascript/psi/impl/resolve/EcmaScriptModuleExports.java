/*
 * Copyright 2013-2026 consulo.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package consulo.javascript.ecmascript.psi.impl.resolve;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript.psi.ES6ExportDeclaration;
import consulo.javascript.ecmascript.psi.ES6ExportDefaultAssignment;
import consulo.javascript.ecmascript.psi.ES6ExportSpecifier;
import consulo.javascript.ecmascript.psi.ES6ImportDeclaration;
import consulo.javascript.ecmascript.psi.ES6ImportedBinding;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.module.resolve.JSModuleExport;
import consulo.javascript.module.resolve.JavaScriptModuleExports;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.javascript.module.resolve.JavaScriptModuleResolution;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.PsiFileImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.util.PsiTreeUtil;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The exports of an ES module in plain JavaScript, by the export table of its file stub - the module isn't parsed when an
 * import resolves:
 * <ul>
 * <li>{@code export function f}, {@code export class C}, {@code export const a};</li>
 * <li>{@code export default ...};</li>
 * <li>{@code export { a, b as c }} of local declarations or of imported names (re-exports);</li>
 * <li>{@code export { a } from './x'}, {@code export * from './x'}, {@code export * as ns from './x'}.</li>
 * </ul>
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class EcmaScriptModuleExports implements JavaScriptModuleExports {
    @Override
    public boolean accepts(PsiFile module) {
        return module instanceof JSFile && module.getLanguage() == JavaScriptLanguage.INSTANCE;
    }

    @RequiredReadAction
    @Override
    public void collectExports(PsiFile module, String name, List<PsiElement> result, Set<JavaScriptModuleExports.Visit> visited) {
        if (!(module instanceof JSFile file)) {
            return;
        }
        for (JSModuleExport export : getExports(file)) {
            switch (export.kind()) {
                case LOCAL -> {
                    if (name.equals(export.name())) {
                        String localName = export.localName();
                        if (localName != null) {
                            result.addAll(findTopLevelDeclarations(file, localName));
                        }
                        else {
                            addDefaultExpression(file, result);
                        }
                    }
                }
                case REEXPORT -> {
                    String localName = export.localName();
                    PsiFile target = name.equals(export.name()) && localName != null ? resolveModule(file, export.specifier()) : null;
                    if (target != null) {
                        if (JSModuleExport.NAMESPACE_EXPORT.equals(localName)) {
                            result.add(target);
                        }
                        else {
                            JavaScriptModuleExports.collect(target, localName, result, visited);
                        }
                    }
                }
                case STAR -> {
                    // export * from './x' re-exports the named exports, not the default one
                    PsiFile target = DEFAULT_EXPORT.equals(name) ? null : resolveModule(file, export.specifier());
                    if (target != null) {
                        JavaScriptModuleExports.collect(target, name, result, visited);
                    }
                }
                case EQUALS -> {
                }
            }
        }
    }

    /**
     * The exports of the module: by its stub, by the tree when it's loaded
     */
    @RequiredReadAction
    private static List<JSModuleExport> getExports(JSFile file) {
        StubElement<?> stub = getStub(file);
        return stub instanceof JSFileStub fileStub ? fileStub.getExports() : buildExports(file);
    }

    /**
     * The exports of a module from its tree, for its stub: an imported name exported again is a re-export of its module
     */
    @RequiredReadAction
    public static List<JSModuleExport> buildExports(JSFile file) {
        PsiElement[] statements = file.getNode().getPsi().getChildren();

        // the imported names: local name -> exported name and module
        Map<String, String[]> imports = new HashMap<>();
        for (PsiElement statement : statements) {
            if (statement instanceof ES6ImportDeclaration declaration) {
                for (ES6ImportedBinding binding : PsiTreeUtil.findChildrenOfType(declaration, ES6ImportedBinding.class)) {
                    String localName = binding.getName();
                    String importedName = binding.getImportedName();
                    String specifier = binding.getModuleSpecifier();
                    if (localName != null && importedName != null && specifier != null) {
                        imports.put(localName, new String[]{importedName, specifier});
                    }
                }
            }
        }

        List<JSModuleExport> result = new ArrayList<>();
        for (PsiElement statement : statements) {
            if (statement instanceof ES6ExportDefaultAssignment assignment) {
                PsiNamedElement declaration = PsiTreeUtil.getChildOfAnyType(assignment, JSFunction.class, JSClass.class);
                String localName = declaration == null ? null : declaration.getName();
                result.add(JSModuleExport.local(DEFAULT_EXPORT, localName));
            }
            else if (statement instanceof ES6ExportDeclaration declaration) {
                addExports(declaration, imports, result);
            }
            else if (isExported(statement)) {
                for (String declared : declaredNames(statement)) {
                    result.add(JSModuleExport.local(declared, declared));
                }
            }
        }
        return result;
    }

    @RequiredReadAction
    private static void addExports(ES6ExportDeclaration declaration, Map<String, String[]> imports, List<JSModuleExport> result) {
        String specifier = declaration.getModuleSpecifier();
        if (declaration.isExportAll()) {
            if (specifier == null) {
                return;
            }
            String namespace = declaration.getNamespaceName();
            result.add(namespace != null ? JSModuleExport.reexport(namespace, JSModuleExport.NAMESPACE_EXPORT, specifier) : JSModuleExport.star(specifier));
            return;
        }
        for (ES6ExportSpecifier exportSpecifier : declaration.getSpecifiers()) {
            String localName = exportSpecifier.getLocalName();
            String exportedName = exportSpecifier.getExportedName();
            if (localName == null || exportedName == null) {
                continue;
            }
            if (specifier != null) {
                result.add(JSModuleExport.reexport(exportedName, localName, specifier));
                continue;
            }
            String[] imported = imports.get(localName);
            result.add(imported != null
                ? JSModuleExport.reexport(exportedName, imported[0], imported[1])
                : JSModuleExport.local(exportedName, localName));
        }
    }

    private static List<String> declaredNames(PsiElement statement) {
        if (statement instanceof JSFunction function) {
            return function.getName() == null ? List.of() : List.of(function.getName());
        }
        if (statement instanceof JSClass jsClass) {
            return jsClass.getName() == null ? List.of() : List.of(jsClass.getName());
        }
        if (statement instanceof JSVarStatement varStatement) {
            List<String> names = new ArrayList<>();
            for (JSVariable variable : varStatement.getVariables()) {
                if (variable.getName() != null) {
                    names.add(variable.getName());
                }
            }
            return names;
        }
        return List.of();
    }

    /**
     * {@code export function}, {@code export class}, {@code export const}: the export keyword in the attribute list
     */
    private static boolean isExported(PsiElement statement) {
        ASTNode attributeList = statement.getNode().findChildByType(JSElementTypes.ATTRIBUTE_LIST);
        return attributeList != null && attributeList.findChildByType(JSTokenTypes.EXPORT_KEYWORD) != null;
    }

    /**
     * The top level functions, classes and variables of a name: by the stub of the file (a declaration of
     * {@code export default} is a stub of the file too)
     */
    @RequiredReadAction
    private static List<PsiElement> findTopLevelDeclarations(JSFile file, String name) {
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
                else if ((element instanceof JSFunction || element instanceof JSClass) && name.equals(((PsiNamedElement) element).getName())) {
                    result.add(element);
                }
            }
            return result;
        }
        for (PsiElement statement : file.getNode().getPsi().getChildren()) {
            PsiElement declaration = statement instanceof ES6ExportDefaultAssignment assignment
                ? PsiTreeUtil.getChildOfAnyType(assignment, JSFunction.class, JSClass.class)
                : statement;
            if (declaration instanceof JSVarStatement varStatement) {
                for (JSVariable variable : varStatement.getVariables()) {
                    if (name.equals(variable.getName())) {
                        result.add(variable);
                    }
                }
            }
            else if ((declaration instanceof JSFunction || declaration instanceof JSClass) && name.equals(((PsiNamedElement) declaration).getName())) {
                result.add(declaration);
            }
        }
        return result;
    }

    /**
     * {@code export default <expression>}: the statement, by the tree
     */
    @RequiredReadAction
    private static void addDefaultExpression(JSFile file, List<PsiElement> result) {
        for (PsiElement statement : file.getNode().getPsi().getChildren()) {
            if (statement instanceof ES6ExportDefaultAssignment assignment) {
                result.add(assignment);
            }
        }
    }

    /**
     * The stub of a file whose tree isn't loaded (the green stub, else the stub from the index)
     */
    private static @Nullable StubElement<?> getStub(PsiFile file) {
        if (!(file instanceof PsiFileImpl fileImpl)) {
            return null;
        }
        StubElement<?> stub = fileImpl.getGreenStub();
        return stub != null ? stub : fileImpl.getStub();
    }

    @RequiredReadAction
    private static @Nullable PsiFile resolveModule(PsiElement context, @Nullable String specifier) {
        if (specifier == null) {
            return null;
        }
        PsiFile module = JavaScriptModuleResolution.resolve(context, specifier, JavaScriptModuleRequest.Kind.IMPORT);
        return module != null ? module : JavaScriptModuleResolution.resolve(context, specifier, JavaScriptModuleRequest.Kind.TYPES);
    }
}
