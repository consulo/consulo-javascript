/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.JavaScriptVersionUtil;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.editor.refactoring.NamesValidator;
import consulo.language.editor.util.PsiUtilBase;
import consulo.language.psi.*;
import consulo.language.psi.resolve.ResolveCache;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.util.collection.ArrayUtil;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlToken;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Set;

public class JSReferenceExpressionImpl extends JSExpressionImpl implements JSReferenceExpression, BindablePsiReference {
    private static final TokenSet IDENTIFIER_TOKENS_SET =
        TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.ANY_IDENTIFIER));

    public JSReferenceExpressionImpl(ASTNode node) {
        super(node);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JavaScriptType getType() {
        PsiElement resolvedElement = resolve();
        if (resolvedElement instanceof JSVariable variable) {
            return variable.getType();
        }
        else if (resolvedElement instanceof JSFunction function) {
            return function.getReturnType();
        }
        return super.getType();
    }

    @Override
    @Nullable
    @RequiredReadAction
    public JSExpression getQualifier() {
        return getFirstChild() instanceof JSExpression expression ? expression : null;
    }

    @Override
    @Nullable
    @RequiredReadAction
    public String getReferencedName() {
        PsiElement nameElement = getNameElement();
        return nameElement != null ? nameElement.getText() : null;
    }

    @Override
    @Nullable
    @RequiredReadAction
    public PsiElement getReferenceNameElement() {
        PsiElement element = getNameElement();
        return element != null ? element : null;
    }

    @RequiredReadAction
    @Override
    public PsiElement getElement() {
        return this;
    }

    @Override
    public PsiReference getReference() {
        return this;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public TextRange getRangeInElement() {
        PsiElement nameElement = getNameElement();

        if (nameElement != null) {
            int offset = nameElement.getStartOffsetInParent();
            return new TextRange(offset, offset + nameElement.getTextLength());
        }
        else {
            return new TextRange(0, getTextLength());
        }
    }

    @Nullable
    @RequiredReadAction
    private PsiElement getNameElement() {
        return findChildByType(IDENTIFIER_TOKENS_SET);
    }

    @Override
    @RequiredReadAction
    public PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(true);

        return resolveResults.length == 0 || resolveResults.length > 1 ? null : resolveResults[0].getElement();
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String getCanonicalText() {
        return getText();
    }

    @Override
    @RequiredWriteAction
    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        return this;
    }

    @RequiredWriteAction
    PsiElement handleElementRenameInternal(String newElementName) throws IncorrectOperationException {
        int i = newElementName.lastIndexOf('.');
        if (i != -1) {
            newElementName = newElementName.substring(0, i);
        }
        if (!NamesValidator.forLanguage(JavaScriptLanguage.INSTANCE).isIdentifier(newElementName, null)) {
            throw new IncorrectOperationException("Invalid javascript element name:" + newElementName);
        }
        PsiElement parent = getParent();
        if (parent instanceof JSClass || parent instanceof JSFunction) {
            PsiElement node = ((JSNamedElement)parent).getNameIdentifier();
            if (node != null && node == this) {
                return this; // JSNamedElement.setName will care of things
            }
        }
        JSChangeUtil.doIdentifierReplacement(this, getNameElement(), newElementName);
        return getParent();
    }

    @Override
    @RequiredReadAction
    public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
        PsiElement parent = getParent();

        if (parent instanceof JSClass || parent instanceof JSNamespaceDeclaration || parent instanceof JSFunction) {
            PsiElement node = ((JSNamedElement)parent).getNameIdentifier();

            if (node != null && node == this && (parent == element || element instanceof PsiFile)) {
                return this; // JSNamedElement.setName will care of things
            }
        }

        String qName = JSPsiImplUtils.getQNameForMove(this, element);

        if (qName != null) {
            PsiElement newChild = JSChangeUtil.createExpressionFromText(getProject(), qName);
            getParent().getNode().replaceChild(getNode(), newChild.getNode());
            return newChild;
        }

        String newName = ((PsiNamedElement)element).getName();
        if (element instanceof PsiFile) {
            int index = newName.lastIndexOf('.');
            if (index != -1) {
                newName = newName.substring(0, index);
            }
        }

        ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), newName);
        getNode().replaceChild(getNameElement().getNode(), nameElement);
        return this;
    }

    @Override
    @RequiredReadAction
    public boolean isReferenceTo(PsiElement element) {
        ResolveHelper helper = ResolveHelper.find(this);
        if (helper.isResolveTo(this, element)) {
            return true;
        }

        PsiElement resolvedElement = resolve();
        if (resolvedElement != null && resolvedElement.isEquivalentTo(element)) {
            return true;
        }

//        if (element instanceof PsiNamedElement || element instanceof XmlAttributeValue) {
//            String referencedName = getReferencedName();
//
//            if (referencedName != null) {
//                if (element instanceof JSDefinitionExpression definition && referencedName.equals(definition.getName())) {
//                    if (definition.getExpression() instanceof JSReferenceExpression jsReferenceExpression) {
//                        JSExpression qualifier = jsReferenceExpression.getQualifier();
//                        JSExpression myQualifier = getQualifier();
//
//                        return (myQualifier != null || (qualifier == myQualifier || "window".equals(qualifier.getText())));
//                    }
//                    else {
//                        return true;
//                    }
//                }
//                else if (element instanceof JSProperty property && referencedName.equals(property.getName())) {
//                    if (getQualifier() != null) {
//                        return true; // TODO: check for type of element to be the same
//                    }
//                    //return false;
//                }
//            }
//            return JSResolveUtil.isReferenceTo(this, referencedName, element);
//        }
        return false;
    }

    @RequiredReadAction
    private void doProcessLocalDeclarations(
        JSExpression qualifier,
        ResolveProcessor processor,
        Set<JavaScriptFeature> features,
        boolean completion
    ) {
        JSClass jsClass = findEnclosingClass(this);
        processor.configureClassScope(jsClass);

        boolean inTypeContext = JSResolveUtil.isExprInTypeContext(this);
        boolean whereTypeCanBe = inTypeContext
            || (completion && features.contains(JavaScriptFeature.CLASS) && JSResolveUtil.isInPlaceWhereTypeCanBeDuringCompletion(this));
        PsiElement elToProcess = this;
        PsiElement scopeToStopAt = null;

        PsiElement parent = getParent();
        boolean strictClassOffset = JSResolveUtil.getTopReferenceParent(parent) instanceof JSImportStatement;
        boolean toProcessMembers = !strictClassOffset;

        if (qualifier != null) {
            elToProcess = jsClass;

            if (jsClass == null) {
                if (qualifier instanceof JSThisExpression) {
                    if (features.contains(JavaScriptFeature.CLASS)) {
                        JSFunction nearestFunction = PsiTreeUtil.getParentOfType(this, JSFunction.class);
                        elToProcess = nearestFunction != null ? nearestFunction : this;
                    }
                    else {
                        elToProcess = PsiTreeUtil.getParentOfType(this, JSProperty.class);
                        if (elToProcess != null) {
                            scopeToStopAt = elToProcess.getParent();
                        }
                    }
                }
                else if (qualifier instanceof JSSuperExpression) {
                    elToProcess = JSResolveUtil.getClassFromTagNameInMxml(this);
                }
            }
        }
        else if (whereTypeCanBe) {
            if (inTypeContext) {
                if (!(parent instanceof JSNewExpression || parent instanceof JSAttributeList || parent instanceof JSBinaryExpression)) {
                    toProcessMembers = false;
                    // get function since it can have imports
                    JSFunction nearestFunction = PsiTreeUtil.getParentOfType(this, JSFunction.class);
                    elToProcess = nearestFunction != null ? nearestFunction.getFirstChild() : jsClass;
                }
            }
            else if (parent instanceof JSExpressionStatement expression && JSResolveUtil.isPlaceWhereNsCanBe(expression)) {
                toProcessMembers = false;
                elToProcess = null;
            }
        }

        if ((qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression) && jsClass != null) {
            scopeToStopAt = jsClass;
            if (features.contains(JavaScriptFeature.CLASS)) {
                JSFunctionExpression expression = PsiTreeUtil.getParentOfType(this, JSFunctionExpression.class);
                if (expression != null) {
                    elToProcess = expression.getFirstChild();
                }
            }
        }

        if (elToProcess == null && whereTypeCanBe) {
            elToProcess = PsiTreeUtil.getParentOfType(this, JSPackageStatement.class, JSFile.class);
            if (elToProcess != null) {
                elToProcess = PsiTreeUtil.getChildOfType(elToProcess, PsiWhiteSpace.class);  // this is hack, get rid of it
                if (elToProcess == null) {
                    elToProcess = this;
                }
            }
        }

        processor.setTypeContext(whereTypeCanBe || (qualifier == null && parent instanceof JSReferenceExpression) || strictClassOffset);
        processor.setToProcessMembers(toProcessMembers);

        if (elToProcess != null) {
            processor.setToProcessHierarchy(qualifier != null || !inTypeContext);
            processor.setToSkipClassDeclarationsOnce(qualifier instanceof JSSuperExpression);
            JSResolveUtil.treeWalkUp(processor, elToProcess, elToProcess, this, scopeToStopAt);

            processor.setToProcessHierarchy(false);
            processor.setToSkipClassDeclarationsOnce(false);
        }
    }

    @Nullable
    private static JSClass findEnclosingClass(PsiElement elt) {
        JSClass jsClass = PsiTreeUtil.getParentOfType(elt, JSClass.class);
        if (jsClass == null && elt != null) {
            PsiElement element = JSResolveUtil.getClassReferenceForXmlFromContext(elt.getContainingFile());
            if (element instanceof JSClass jsClassClass) {
                jsClass = jsClassClass;
            }
        }
        return jsClass;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public Object[] getVariants() {
        PsiFile containingFile = getContainingFile();
        Set<JavaScriptFeature> features = JavaScriptVersionUtil.getFeatures(this);
        boolean classFeature = features.contains(JavaScriptFeature.CLASS);
        Object[] smartVariants = JSSmartCompletionVariantsHandler.getSmartVariants(this, classFeature);
        if (smartVariants != null) {
            return smartVariants;
        }
        JSExpression qualifier = getResolveQualifier();

        if (qualifier == null) {
            if (JSResolveUtil.isSelfReference(getParent(), this)) { // Prevent Rulezz to appear
                return ArrayUtil.EMPTY_OBJECT_ARRAY;
            }

            ResolveProcessor localProcessor = new ResolveProcessor(null, this);

            doProcessLocalDeclarations(qualifier, localProcessor, features, true);
            VariantsProcessor processor = new VariantsProcessor(null, containingFile, false, this);

            processor.addLocalResults(localProcessor.getResults());

            JSResolveUtil.processGlobalSymbols(this, processor);

            return processor.getResult();
        }
        else {
            MyTypeProcessor processor = new MyTypeProcessor(null, features, this);
            BaseJSSymbolProcessor.doEvalForExpr(BaseJSSymbolProcessor.getOriginalQualifier(qualifier), containingFile, processor);

            if (processor.resolved == MyTypeProcessor.TypeResolveState.Resolved ||
                processor.resolved == MyTypeProcessor.TypeResolveState.Undefined ||
                (processor.resolved == MyTypeProcessor.TypeResolveState.PrefixUnknown && classFeature)) {
                String qualifiedNameToSkip = null;
                if (JSResolveUtil.isSelfReference(getParent(), this)) {
                    PsiElement originalParent = PsiUtilBase.getOriginalElement(getParent(), JSQualifiedNamedElement.class);
                    if (originalParent instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                        qualifiedNameToSkip = qualifiedNamedElement.getQualifiedName();
                    }
                }
                return processor.getResultsAsObjects(qualifiedNameToSkip);
            }

            return processor.getResultsAsObjects();
        }
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSReferenceExpression(this);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        return ResolveCache.getInstance(getContainingFile().getProject())
            .resolveWithCaching(this, MyResolver.INSTANCE, true, incompleteCode);
    }

    private static class MyResolver implements ResolveCache.PolyVariantResolver<JSReferenceExpressionImpl> {
        private static final MyResolver INSTANCE = new MyResolver();

        @Nonnull
        @Override
        @RequiredReadAction
        public ResolveResult[] resolve(@Nonnull JSReferenceExpressionImpl referenceExpression, boolean incompleteCode) {
            return referenceExpression.doResolve();
        }
    }

    @RequiredReadAction
    private ResolveResult[] doResolve() {
        PsiFile containingFile = getContainingFile();
        String referencedName = getReferencedName();
        if (referencedName == null) {
            return ResolveResult.EMPTY_ARRAY;
        }

        PsiElement parent = getParent();
        JSExpression qualifier = getResolveQualifier();
        Set<JavaScriptFeature> features = JavaScriptVersionUtil.getFeatures(this);
        boolean classFeature = features.contains(JavaScriptFeature.CLASS);

        boolean localResolve = qualifier == null;
        boolean parentIsDefinition = parent instanceof JSDefinitionExpression;

        // Handle self references
        PsiElement currentParent = JSResolveUtil.getTopReferenceParent(parent);
        if (JSResolveUtil.isSelfReference(currentParent, this)
            && (!(currentParent instanceof JSPackageStatement) || parent == currentParent)) {
            return new ResolveResult[]{new JSResolveUtil.MyResolveResult(currentParent)};
        }

        JSExpression realQualifier = getQualifier();
        if (isE4XAttributeReference(realQualifier)) { // TODO: fix tree
            return new ResolveResult[]{new JSResolveUtil.MyResolveResult(this)};
        }

        if ("*".equals(referencedName) && currentParent instanceof JSImportStatement
            && qualifier instanceof JSReferenceExpression refExpr) { // TODO: move to some processor
            return refExpr.multiResolve(false);
        }

        ResolveProcessor localProcessor;

        if (qualifier == null) {
            localProcessor = new ResolveProcessor(referencedName, this);

            boolean canResolveAllLocally = !parentIsDefinition || !classFeature;
            doProcessLocalDeclarations(realQualifier, localProcessor, features, false);

            if (canResolveAllLocally) {
                PsiElement jsElement = localProcessor.getResult();

                if (jsElement != null || (qualifier != null && classFeature && localProcessor.foundAllValidResults())) {
                    return localProcessor.getResultsAsResolveResults();
                }
            }
            return doOldResolve(
                containingFile,
                referencedName,
                parent,
                qualifier,
                classFeature,
                localResolve,
                parentIsDefinition,
                localProcessor
            );
        }
        else {
            MyTypeProcessor processor = new MyTypeProcessor(referencedName, features, this);
            BaseJSSymbolProcessor.doEvalForExpr(qualifier, containingFile, processor);

            if (processor.resolved == MyTypeProcessor.TypeResolveState.PrefixUnknown && classFeature) {
                return new ResolveResult[]{new JSResolveUtil.MyResolveResult(this)};
            }

            if (processor.resolved == MyTypeProcessor.TypeResolveState.Resolved ||
                processor.resolved == MyTypeProcessor.TypeResolveState.Undefined ||
                processor.getResult() != null) {
                return processor.getResultsAsResolveResults();
            }
            else {
                return processor.getResultsAsResolveResults();
            }
        }
    }

    @RequiredReadAction
    private boolean isE4XAttributeReference(JSExpression realQualifier) {
        return getNode().findChildByType(JSTokenTypes.AT) != null
            || (realQualifier != null && realQualifier.getNode().findChildByType(JSTokenTypes.AT) != null);
    }

    @Nullable
    @RequiredReadAction
    public JSExpression getResolveQualifier() {
        JSExpression qualifier = getQualifier();

        if (qualifier instanceof JSReferenceExpression) {
            ASTNode astNode = getNode();
            ASTNode selection = astNode.getTreeNext();
            // TODO:this is not accurate
            if (selection != null && selection.getElementType() == JSTokenTypes.COLON_COLON) {
                return null;
            }

            ASTNode nsSelection = astNode.findChildByType(JSTokenTypes.COLON_COLON);
            if (nsSelection != null) {
                return ((JSReferenceExpressionImpl)qualifier).getResolveQualifier();
            }
        }
        else if (qualifier == null) {
            ASTNode node = getNode().getFirstChildNode();

            if (node.getElementType() == JSTokenTypes.AT) {
                PsiElement parent = getParent();
                if (parent instanceof JSBinaryExpression binary && binary.getParent().getParent() instanceof JSCallExpression call) {
                    parent = call.getMethodExpression();
                }
                return parent instanceof JSExpression expression ? expression : null;
            }
        }
        return qualifier;
    }

    @RequiredReadAction
    private ResolveResult[] doOldResolve(
        PsiFile containingFile,
        String referencedName,
        PsiElement parent,
        JSExpression qualifier,
        boolean ecma,
        boolean localResolve,
        boolean parentIsDefinition,
        ResolveProcessor localProcessor
    ) {
        if (parentIsDefinition && ((ecma && !localResolve) || (!ecma && qualifier != null))) {
            return new ResolveResult[]{new JSResolveUtil.MyResolveResult(parent)};
        }

        if (localResolve && parentIsDefinition && ecma) {
            if (!localProcessor.processingEncounteredAbsenceOfTypes()) {
                return localProcessor.getResultsAsResolveResults();
            }

            // Fallback for finding some assignment in global scope
        }

        WalkUpResolveProcessor processor =
            new WalkUpResolveProcessor(referencedName, null, containingFile, false, this);

        if (localProcessor != null) {
            processor.addLocalResults(localProcessor.getResultsAsResolveResults());
        }

        JSResolveUtil.processGlobalSymbols(this, processor);
        return processor.getResults();
    }

    @Override
    public boolean shouldCheckReferences() {
        return true;
    }

    private static class MyTypeProcessor extends ResolveProcessor implements BaseJSSymbolProcessor.TypeProcessor {
        private final Set<JavaScriptFeature> myFeatures;

        public MyTypeProcessor(String referenceName, Set<JavaScriptFeature> features, PsiElement _place) {
            super(referenceName, _place);
            myFeatures = features;
            setToProcessHierarchy(true);

            configureClassScope(findEnclosingClass(_place));
        }

        enum TypeResolveState {
            Unknown,
            Resolved,
            Undefined,
            PrefixUnknown
        }

        TypeResolveState resolved = TypeResolveState.Unknown;

        @Override
        public Set<JavaScriptFeature> getFeatures() {
            return myFeatures;
        }

        @Override
        @RequiredReadAction
        public void process(@Nonnull String type, @Nonnull BaseJSSymbolProcessor.EvaluateContext evaluateContext, PsiElement source) {
            if (evaluateContext.visitedTypes.contains(type)) {
                return;
            }
            evaluateContext.visitedTypes.add(type);

            if ("*".equals(type) || "Object".equals(type)) {
                return;
            }

            if (JSTypeEvaluateManager.isArrayType(type)) {
                int index = type.indexOf('[');
                if (index != -1) {
                    type = type.substring(0, index);
                }
            }

            PsiElement typeSource = evaluateContext.getSource();

            setProcessStatics(false);

            PsiElement placeParent = place.getParent();
            boolean setTypeContext = placeParent instanceof JSReferenceList;
            PsiElement clazz = source != null && (source instanceof JSClass || source instanceof XmlFile)
                ? source
                : JSClassImpl.findClassFromNamespace(type, place);

            if (clazz instanceof JSClass jsClass) {
                boolean statics = ecma() && JSPsiImplUtils.isTheSameClass(typeSource, jsClass)
                    && !(((JSReferenceExpression)place).getQualifier() instanceof JSCallExpression);
                setProcessStatics(statics);
                if (statics) {
                    setTypeName(jsClass.getQualifiedName());
                }

                boolean saveSetTypeContext = isTypeContext();
                boolean saveToProcessMembers = isToProcessMembers();

                if (setTypeContext) {
                    setTypeContext(setTypeContext);
                    setToProcessMembers(false);
                }

                try {
                    boolean b = clazz.processDeclarations(this, ResolveState.initial(), clazz, place);
                    if (!b) {
                        resolved = TypeResolveState.Resolved;
                    }
                    else if (ecma()) {
                        JSAttributeList attrList = jsClass.getAttributeList();
                        if (attrList == null || !attrList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
                            resolved = TypeResolveState.Resolved;
                        }
                    }
                }
                finally {
                    if (setTypeContext) {
                        setTypeContext(saveSetTypeContext);
                        setToProcessMembers(saveToProcessMembers);
                    }
                }
            }
            else if (ecma()) {
                resolved = TypeResolveState.Undefined;
            }
        }

        @Override
        @RequiredReadAction
        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
            boolean b = super.execute(element, state);
            if (ecma() && getResult() != null) {
                resolved = MyTypeProcessor.TypeResolveState.Resolved;
            }
            return b;
        }

        @Override
        public boolean ecma() {
            return myFeatures.contains(JavaScriptFeature.CLASS);
        }

        @Override
        public void setUnknownElement(@Nonnull PsiElement element) {
            if (!(element instanceof XmlToken)) {
                boolean currentIsNotResolved =
                    element == BaseJSSymbolProcessor.getOriginalQualifier(((JSReferenceExpression)place).getQualifier());
                resolved = currentIsNotResolved ? TypeResolveState.PrefixUnknown : TypeResolveState.Unknown;
            }
        }
    }
}
