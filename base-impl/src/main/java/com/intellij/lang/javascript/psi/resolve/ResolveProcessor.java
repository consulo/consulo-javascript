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

package com.intellij.lang.javascript.psi.resolve;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.lang.javascript.validation.JSUnusedImportsHelper;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.util.collection.ArrayUtil;
import consulo.util.collection.SmartList;
import consulo.util.dataholder.Key;
import consulo.util.dataholder.UserDataHolderBase;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlTag;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Maxim.Mossienko
 */
public class ResolveProcessor extends UserDataHolderBase implements PsiScopeProcessor {
    private static final Key<String> ASKING_FOR_QUALIFIED_IMPORT = Key.create("asking.for.import.of.qname");
    protected static final Key<Boolean> LOOKING_FOR_USE_NAMESPACES = Key.create("looking.for.use.directive");

    private final Set<String> visitedClasses = new HashSet<>();

    private boolean myToStopOnAssignment;
    protected final String myName;
    private PsiElement myResult;
    private PsiElement myCandidateResult;
    private List<PsiElement> myResults;
    private List<JavaScriptImportStatementBase> myImportsUsed;
    private List<Boolean> myResolveStatus;

    private boolean toProcessHierarchy;
    private boolean toSkipClassDeclarationOnce;
    private boolean toProcessMembers = true;
    private boolean encounteredDynamicClasses;
    private boolean encounteredDynamicClassesSet;
    private boolean encounteredFunctionExpression;
    private boolean processStatics;
    private boolean acceptPrivateMembers = true;
    private boolean acceptProtectedMembers = true;
    private boolean acceptProtectedMembersSet;
    private boolean allowUnqualifiedStaticsFromInstance;

    private boolean myTypeContext;
    private boolean encounteredWithStatement;
    private boolean localResolve;
    protected final PsiElement place;
    private String myTypeName;
    private String myClassScopeTypeName;

    private Set<String> openedNses;
    private boolean defaultNsIsNotAllowed;
    private boolean anyNsAllowed;
    private boolean myAcceptOnlyClasses;
    private boolean myAcceptOnlyInterfaces;

    public static final Key<JavaScriptImportStatementBase> IMPORT_KEY = Key.create("import.key");
    private boolean myClassDeclarationStarted;
    private PsiElement placeTopParent;
    protected boolean ecma;
    public static final String AS3_NAMESPACE = "AS3";

    private ResolveHelper myResolveHelper;

    public ResolveProcessor(String name) {
        this(name, false);
    }

    public ResolveProcessor(String name, boolean toStopOnAssignment) {
        myName = name;
        myToStopOnAssignment = toStopOnAssignment;
        place = null;
    }

    public ResolveProcessor(String name, PsiElement _place) {
        myName = name;
        place = _place;
        boolean b = place instanceof JSReferenceExpression && ((JSReferenceExpression)place).getQualifier() == null;
        allowUnqualifiedStaticsFromInstance = b;

        if (myName != null && place instanceof JSReferenceExpression placeRefExpr) {
            ASTNode node = place.getNode().findChildByType(JSTokenTypes.COLON_COLON);
            String explicitNs = null;

            // TODO: e.g. protected is also ns
            JSExpression qualifier = placeRefExpr.getQualifier();
            if (node != null) {
                if (qualifier instanceof JSReferenceExpression qualifierRefExpr) {
                    anyNsAllowed = qualifierRefExpr.resolve() instanceof JSVariable;
                }

                explicitNs = qualifier != null ? qualifier.getText() : null;
            }
            else if (qualifier instanceof JSReferenceExpression qualifierRefExpr
                && qualifier.getNode().findChildByType(JSTokenTypes.COLON_COLON) != null) {
                anyNsAllowed = qualifierRefExpr.resolve() instanceof JSVariable;
                explicitNs = qualifierRefExpr.getReferencedName();
            }

            if (explicitNs != null && !anyNsAllowed) {
                openedNses = new HashSet<>();
                openedNses.add(explicitNs);
                defaultNsIsNotAllowed = true;
            }
        }

        if (myName == null && place instanceof JSReferenceExpression) {
            PsiElement parent = place.getParent();

            if (parent instanceof JSReferenceList) {
                if (parent.getNode().getElementType() == JSElementTypes.EXTENDS_LIST) {
                    PsiElement element = parent.getParent();
                    if (element instanceof JSClass jsClass) {
                        if (jsClass.isInterface()) {
                            myAcceptOnlyClasses = false;
                            myAcceptOnlyInterfaces = true;
                        }
                        else {
                            myAcceptOnlyClasses = true;
                            myAcceptOnlyInterfaces = false;
                        }
                    }
                }
                else {
                    myAcceptOnlyClasses = false;
                    myAcceptOnlyInterfaces = true;
                }
            }
        }

        ecma = place != null && place.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
    }

    public PsiElement getResult() {
        if (myResult == null && myCandidateResult != null) {
            assert myResults == null;
            return myCandidateResult;
        }
        return myResult;
    }

    public JavaScriptImportStatementBase getImportUsed() {
        return myImportsUsed != null ? myImportsUsed.get(0) : null;
    }

    public List<PsiElement> getResults() {
        if (myResults == null && myResult == null && myCandidateResult != null) {
            myResults = new ArrayList<>(1);
            myResults.add(myCandidateResult);
        }
        return myResults;
    }

    @Override
    @RequiredReadAction
    public boolean execute(@Nonnull PsiElement element, ResolveState state) {
        if ((element instanceof JSVariable && !(element instanceof JSParameter)) || element instanceof JSFunction) {
            JSAttributeList attributeList = ((JSAttributeListOwner)element).getAttributeList();

            // TODO: we should accept such values during resolve but make them invalid
            if (!acceptPrivateMembers
                && attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE) {
                return true;
            }

            if (!acceptProtectedMembers
                && attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PROTECTED) {
                return true;
            }

            if (myClassDeclarationStarted) {
                if (processStatics) {
                    if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                        return true;
                    }
                }
                else if (!allowUnqualifiedStaticsFromInstance && attributeList != null
                    && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                    return true;
                }
            }

            String attributeNs = attributeList != null ? attributeList.getNamespace() : null;

            if (openedNses == null && attributeNs != null && !anyNsAllowed
                && place instanceof JSReferenceExpression placeRefExpr && !JSResolveUtil.isExprInTypeContext(placeRefExpr)) {
                openedNses = JSResolveUtil.calculateOpenNses(place);
            }

            if (openedNses != null
                && !openedNses.contains(attributeNs)
                && (!AS3_NAMESPACE.equals(attributeNs) || !ecma) // AS3 is opened by default from compiler settings
            ) {
                if (attributeNs != null || defaultNsIsNotAllowed) {
                    return true;
                }
            }
        }

        if (myAcceptOnlyClasses || myAcceptOnlyInterfaces) {
            if (element instanceof JSClass jsClass) {
                boolean isInterface = jsClass.isInterface();

                if ((myAcceptOnlyClasses && isInterface) || (myAcceptOnlyInterfaces && !isInterface)) {
                    return true;
                }
            }
        }

        if (place != null) {
            if (myResolveHelper == null) {
                myResolveHelper = ResolveHelper.find(place);
            }

            if (myResolveHelper.execute(this, element, state)) {
                myCandidateResult = element;
                return true;
            }
        }

        if (place != null && element instanceof JSFunction function && !(element instanceof JSFunctionExpression)) {
            PsiElement placeParent = place.getParent();

            if (placeParent instanceof JSDefinitionExpression
                || (myName == null && placeParent instanceof JSExpressionStatement /* when complete of setter*/)
                || (place instanceof JSFunction placeFunction && placeFunction.isSetProperty())) {
                if (function.isGetProperty() && (myName != null
                    || !(placeParent instanceof JSExpressionStatement)
                    || !(function.getParent() instanceof JSClass jsClass
                    && jsClass.findFunctionByNameAndKind(function.getName(), JSFunction.FunctionKind.SETTER) == null))) {
                    return true;
                }
            }
            else if (function.isSetProperty()) {
                return true;
            }
        }

        if (place != null && (place.getParent() instanceof JSNewExpression || place instanceof JSSuperExpression)) {
            if (element instanceof JSClass jsClass) {
                JSFunction byName = jsClass.findFunctionByName(jsClass.getName());
                if (byName != null) {
                    element = byName;
                }
            }
            else if (element instanceof JSFunction function
                && element.getParent() instanceof JSClass /*TODO: in mxml?*/
                && !function.isConstructor() && !function.isGetProperty()) {
                return true;
            }
        }
        else if (element instanceof JSFunction function && function.isConstructor() && !(place instanceof JSClass)) {
            return true;
        }

        if (placeTopParent == null && place != null) {
            placeTopParent = JSResolveUtil.getTopReferenceExpression(place);
        }
        if (placeTopParent instanceof JSReferenceExpression placeTopParentRefExpr
            && JSResolveUtil.isExprInStrictTypeContext(placeTopParentRefExpr)
            && place != placeTopParent
            && (element instanceof JSVariable || element instanceof JSFunction)) {
            return true;
        }

        boolean checkStopOnMatch = false;
        boolean doStopOnMatch = false;

        PsiElement elementToCheck = element;
        if (elementToCheck instanceof JSFunctionExpression
            && elementToCheck.getParent() instanceof JSAssignmentExpression assignment) {
            elementToCheck = assignment.getLOperand();
        }

        if (elementToCheck instanceof JSDefinitionExpression definition) {
            boolean toProcess = false;

            if (definition.getExpression() instanceof JSReferenceExpression refExpr && refExpr.getQualifier() == null) {
                toProcess = true;
            }

            if (!toProcess) {
                return true;
            }
            checkStopOnMatch = true;
            doStopOnMatch = myToStopOnAssignment;
        }

        if (element instanceof PsiNamedElement namedElement && (myName == null || myName.equals(getName(namedElement)))) {
            element = getElement(element);
            if (checkStopOnMatch && !doStopOnMatch) {
                myCandidateResult = element;
            }
            else {
                if (myName != null) {
                    JavaScriptImportStatementBase s = state != null ? state.get(IMPORT_KEY) : null;
                    int previousResultsSize = myResults != null ? myResults.size() : 0;

                    s = checkQualifiedNameHasNecessaryImport(element, s, previousResultsSize);

                    if (s != null || myImportsUsed != null) {
                        if (myImportsUsed == null) {
                            myImportsUsed = new SmartList<>();
                            for (int i = 0; i < previousResultsSize; ++i) {
                                myImportsUsed.add(null);
                            }
                        }

                        myImportsUsed.add(s);
                    }
                }
                if (myResults == null) {
                    myResults = new ArrayList<>(1);
                }
                myResults.add(element);
                myResult = element;
                return myName == null;
            }
        }

        return true;
    }

    private JavaScriptImportStatementBase checkQualifiedNameHasNecessaryImport(
        PsiElement element,
        JavaScriptImportStatementBase s,
        int previousResultsSize
    ) {
        if (s == null && (element instanceof JSClass || element instanceof JSFunction || element instanceof JSVariable
            || element instanceof JSNamespaceDeclaration)) {

            if (placeTopParent instanceof JSReferenceExpression placeTopParentRefExpr
                && !(placeTopParent.getParent() instanceof JavaScriptImportStatementBase)) {
                final String qName = ((JSQualifiedNamedElement)element).getQualifiedName();

                if (qName != null && qName.indexOf('.') != -1) {
                    ResolveProcessor processor = new ResolveProcessor(myName) {
                        @Override
                        @RequiredReadAction
                        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                            if (element instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                                if (!qName.equals(qualifiedNamedElement.getQualifiedName())) {
                                    return true;
                                }
                            }
                            else {
                                return true;
                            }
                            return super.execute(element, state);
                        }
                    };

                    if (!JSUnusedImportsHelper.isSomeNodeThatShouldNotHaveImportsWhenQualified(placeTopParentRefExpr, element)) {
                        processor.putUserData(ASKING_FOR_QUALIFIED_IMPORT, qName);
                        JSResolveUtil.treeWalkUp(processor, placeTopParent, placeTopParent, place);
                        boolean noImportNoResolve = processor.getResult() == null;

                        if (noImportNoResolve) {
                            if (myResolveStatus == null) {
                                myResolveStatus = new SmartList<>();
                                for (int i = 0; i < previousResultsSize; ++i) {
                                    myResolveStatus.add(Boolean.TRUE);
                                }
                            }

                            myResolveStatus.add(Boolean.FALSE);
                        }
                        else {
                            ResolveResult[] resultsAsResolveResults = processor.getResultsAsResolveResults();
                            if (resultsAsResolveResults.length != 0) {
                                s = ((JSResolveUtil.MyResolveResult)resultsAsResolveResults[0]).getImportUsed();
                            }
                        }
                    }
                }
            }
        }
        return s;
    }

    @RequiredReadAction
    private static PsiElement getElement(PsiElement element) {
        return element instanceof XmlTag xmlTag
            ? xmlTag.getAttribute("name").getValueElement().getChildren()[1]
            : element;
    }

    @Nullable
    @RequiredReadAction
    public static String getName(PsiNamedElement element) {
        return element instanceof JSNamedElement ? element.getName() : null;
    }

    @Override
    public <T> T getHint(@Nonnull Key<T> hintClass) {
        return null;
    }

    protected void setTypeName(String qualifiedName) {
        myTypeName = qualifiedName;
    }

    public void configureClassScope(JSClass jsClass) {
        if (jsClass != null) {
            myClassScopeTypeName = jsClass.getQualifiedName();
        }
        else {
            acceptProtectedMembers = false;
            acceptProtectedMembersSet = true;
        }
    }

    @Override
    public void handleEvent(Event event, Object associated) {
        if (event == Event.SET_DECLARATION_HOLDER) {
            if (associated instanceof JSClass) {
                myClassDeclarationStarted = true;
                JSClass jsClass = (JSClass)associated;
                final String qName = jsClass.getQualifiedName();

                if (!encounteredDynamicClassesSet) {
                    JSAttributeList attributeList = jsClass.getAttributeList();
                    if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
                        encounteredDynamicClasses = true;
                    }
                    encounteredDynamicClassesSet = true;
                }

                if (acceptPrivateMembers) {
                    acceptPrivateMembers = qName != null && qName.equals(myClassScopeTypeName);
                }

                if (!acceptProtectedMembersSet) {
                    acceptProtectedMembersSet = true;

                    if (myClassScopeTypeName != null) {
                        acceptProtectedMembers = myClassScopeTypeName.equals(qName);

                        if (!acceptProtectedMembers) {
                            PsiElement element = JSClassImpl.findClassFromNamespace(myClassScopeTypeName, place);
                            if (element instanceof JSClass) {
                                boolean b = element.processDeclarations(
                                    new ResolveProcessor(null) {
                                        {
                                            setTypeContext(true);
                                            setToProcessMembers(false);
                                            setToProcessHierarchy(true);
                                            acceptProtectedMembers = false;
                                            setLocalResolve(true);
                                        }

                                        @Override
                                        @RequiredReadAction
                                        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                                            if (!(element instanceof JSClass jsClass)) {
                                                return true;
                                            }
                                            String classQName = jsClass.getQualifiedName();
                                            return qName == classQName || (qName != null && !qName.equals(classQName));
                                        }
                                    },
                                    ResolveState.initial(),
                                    element,
                                    element
                                );

                                acceptProtectedMembers = !b;
                            }
                        }
                    }
                }

                if (processStatics) {
                    if (myTypeName != null && !myTypeName.equals(jsClass.getQualifiedName())
                        || myTypeName == null && myClassScopeTypeName != null &&
                        !myClassScopeTypeName.equals(jsClass.getQualifiedName())) {
                        processStatics = false;
                    }
                }
            }
            else if (associated instanceof JSAttributeListOwner attributeListOwner) {
                JSAttributeList attributeList = attributeListOwner.getAttributeList();

                if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)
                    && (place == null || !(place.getParent() instanceof JSNewExpression))) {
                    processStatics = true;
                }

                if (associated instanceof JSFunctionExpression functionExpression) {
                    PsiElement context = functionExpression.getContainingFile().getContext();
                    if (!(context instanceof XmlAttributeValue)) {
                        encounteredFunctionExpression = true; // we create anonymous fun exprs for event handlers
                    }
                }
            }
        }
    }

    public String getName() {
        return myName;
    }

    public boolean isToProcessHierarchy() {
        return toProcessHierarchy;
    }

    public void setToProcessHierarchy(boolean toProcessHierarchy) {
        this.toProcessHierarchy = toProcessHierarchy;
    }

    public boolean isToSkipClassDeclarationOnce() {
        return toSkipClassDeclarationOnce;
    }

    public void setToSkipClassDeclarationsOnce(boolean toSkipClassDeclarationOnce) {
        this.toSkipClassDeclarationOnce = toSkipClassDeclarationOnce;
    }

    public boolean foundAllValidResults() {
        return !encounteredFunctionExpression; // TODO: with statement too?
    }

    public void setTypeContext(boolean b) {
        myTypeContext = b;
    }

    public boolean isTypeContext() {
        return myTypeContext;
    }

    public ResolveResult[] getResultsAsResolveResults() {
        List<PsiElement> processorResults = getResults();
        if (processorResults == null) {
            return ResolveResult.EMPTY_ARRAY;
        }
        ResolveResult[] results = new ResolveResult[processorResults.size()];

        for (int i = 0; i < results.length; ++i) {
            PsiElement element = processorResults.get(i);
            results[i] = new JSResolveUtil.MyResolveResult(
                element,
                myImportsUsed != null ? myImportsUsed.get(i) : null,
                myResolveStatus != null ? myResolveStatus.get(i) : true
            );
        }
        return results;
    }

    public boolean processingEncounteredAbsenceOfTypes() {
        return encounteredDynamicClasses || encounteredWithStatement || encounteredFunctionExpression;
    }

    public Object[] getResultsAsObjects() {
        return getResultsAsObjects(null);
    }

    @RequiredReadAction
    public Object[] getResultsAsObjects(String qualifiedNameToSkip) {
        List<PsiElement> processorResults = getResults();
        if (processorResults == null) {
            return ArrayUtil.EMPTY_OBJECT_ARRAY;
        }
        int numberOfVariants = processorResults.size();
        List<Object> objects = new ArrayList<>(numberOfVariants);
        Set<String> processedCandidateNames = new HashSet<>(numberOfVariants);

        for (int i = 0; i < numberOfVariants; ++i) {
            PsiElement namedElement = processorResults.get(i);
            String name = getName((PsiNamedElement)namedElement);
            if (name == null) {
                continue;
            }

            String qName = namedElement instanceof JSQualifiedNamedElement qualifiedNamedElement
                ? qualifiedNamedElement.getQualifiedName()
                : name;

            if (processedCandidateNames.contains(qName)) {
                continue;
            }
            processedCandidateNames.add(qName);

            if (qualifiedNameToSkip != null && qualifiedNameToSkip.equals(qName)) {
                continue;
            }

            objects.add(JSLookupUtil.createLookupItem(namedElement, name, JSLookupUtil.LookupPriority.HIGHEST));
        }
        return ArrayUtil.toObjectArray(objects);
    }

    public boolean isToProcessMembers() {
        return toProcessMembers;
    }

    public void setToProcessMembers(boolean toProcessMembers) {
        this.toProcessMembers = toProcessMembers;
    }

    public boolean checkVisited(String className) {
        if (visitedClasses.contains(className)) {
            return true;
        }
        visitedClasses.add(className);
        return false;
    }

    public void setProcessStatics(boolean processStatics) {
        this.processStatics = processStatics;
    }

    public boolean isLocalResolve() {
        return localResolve;
    }

    public void setLocalResolve(boolean localResolve) {
        this.localResolve = localResolve;
    }

    public boolean specificallyAskingToResolveQualifiedNames() {
        return getUserData(ASKING_FOR_QUALIFIED_IMPORT) != null;
    }

    public String getQualifiedNameToImport() {
        return getUserData(ASKING_FOR_QUALIFIED_IMPORT);
    }

    public boolean lookingForUseNamespaces() {
        return getUserData(LOOKING_FOR_USE_NAMESPACES) != null;
    }

    public boolean isEncounteredDynamicClasses() {
        return encounteredDynamicClasses;
    }
}
