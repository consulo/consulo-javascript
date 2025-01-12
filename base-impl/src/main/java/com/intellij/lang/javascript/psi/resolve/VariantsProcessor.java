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

import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.collection.ArrayUtil;
import jakarta.annotation.Nonnull;

import java.util.*;

/**
 * @author Maxim.Mossienko
 */
public class VariantsProcessor extends BaseJSSymbolProcessor {
    private Map<String, Object> myNames2CandidatesMap = new HashMap<>();
    private int myThisFileNameListCount;
    private List<Object> myNamesList = new ArrayList<>();

    private Map<String, Object> myPartialMatchNamesMap = new HashMap<>();
    private Map<String, Object> myPartialMatchNamesMapFromSameFile = new HashMap<>();
    //private Set<PsiFile> myTargetFiles;
    private final boolean hasSomeSmartnessAvailable;
    private boolean myProcessOnlyTypes;

    private boolean myAddOnlyCompleteMatchesSet;

    private static final String OBJECT_CLASS_NAME = "Object";

    @RequiredReadAction
    public VariantsProcessor(String[] nameIds, PsiFile targetFile, boolean skipDclsInTargetFile, PsiElement context) {
        super(targetFile.getOriginalFile(), skipDclsInTargetFile, context, nameIds);
        nameIds = myContextNameIds;

        boolean hasSomeInfoAvailable = nameIds != null && nameIds.length > 0;
        final List<String[]> possibleNameComponents = new ArrayList<>(1);
        myCurrentFile = targetFile.getOriginalFile();
        //myTargetFiles = new HashSet<PsiFile>();

        boolean allTypesResolved = false;
        boolean doAddContextIds = true;

        final JSClass jsClass = PsiTreeUtil.getParentOfType(context, JSClass.class);

        if (context instanceof JSReferenceExpression) {
            final JSReferenceExpression refExpr = (JSReferenceExpression)context;
            JSExpression qualifier = refExpr.getQualifier();

            if (qualifier != null) {
                if (qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression) {
                    if (jsClass != null) {
                        myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
                        updateCanUseOnlyCompleteMatches(jsClass);
                        doAddContextIds = false;
                    }
                    else if (JSResolveUtil.getTypeFromTagNameInMxml(myCurrentFile.getContext()) != null) {
                        myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
                    }
                }

                final CompletionTypeProcessor processor = new CompletionTypeProcessor(possibleNameComponents);
                doEvalForExpr(getOriginalQualifier(qualifier), myTargetFile, processor);
                allTypesResolved = processor.getAllTypesResolved();
            }
            else {
                final PsiElement parent = refExpr.getParent();

                if (parent instanceof JavaScriptImportStatementBase) {
                    myProcessOnlyTypes = true;
                }
                else if (JSResolveUtil.isExprInTypeContext(refExpr)
                    || (ecmal4 && JSResolveUtil.isInPlaceWhereTypeCanBeDuringCompletion(refExpr))) {
                    myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
                    myProcessOnlyTypes = true;
                    allTypesResolved = true;
                    addPackageScope(possibleNameComponents, jsClass, refExpr);
                }
                else if (jsClass != null) {
                    myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
                    updateCanUseOnlyCompleteMatches(jsClass);
                }
            }
        }

        if (hasSomeInfoAvailable && (!allTypesResolved || (!possibleNameComponents.isEmpty()
            && possibleNameComponents.get(0).length == 1
            && "Function".equals(possibleNameComponents.get(0)[0])))) {
            if (doAddContextIds) {
                possibleNameComponents.add(nameIds);
            }

            doIterateTypeHierarchy(
                nameIds,
                clazz -> {
                    updateCanUseOnlyCompleteMatchesFromString(clazz.getQualifiedName(), clazz, clazz);
                    buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameComponents);
                    return true;
                }
            );
        }

        String[][] nameIdsArray = new String[possibleNameComponents.size()][];
        possibleNameComponents.toArray(nameIdsArray);

        hasSomeSmartnessAvailable = nameIdsArray != null && nameIdsArray.length > 0;
    }

    @RequiredReadAction
    private void updateCanUseOnlyCompleteMatchesFromString(final String qName, Object source, PsiElement clazz) {
        final boolean wasSet = myAddOnlyCompleteMatchesSet;

        if (myAddOnlyCompleteMatches || !wasSet) {
            myAddOnlyCompleteMatchesSet = true;
            if (qName.equals("*")
                || (qName.equals(OBJECT_CLASS_NAME) && (myIteratedTypeName == null || qName.equals(myIteratedTypeName)
                // skip adding complete matches when context name is qualifier
                || (myContextNameIds != null && myContextNameIds.length > 0
                && myIteratedTypeName.equals(myContextNameIds[myContextNameIds.length - 1])
                && myContext instanceof JSReferenceExpression referenceExpression
                && referenceExpression.getQualifier() instanceof JSReferenceExpression))) /* something explicitly marked as Object*/
                || (qName.equals(FUNCTION_TYPE_NAME) && isObjectSourceThatDoesNotGiveExactKnowledgeAboutFunctionType(source))) {
                myAddOnlyCompleteMatches = false;
            }
            else {
                if (clazz == null) {
                    clazz = JSResolveUtil.findClassByQName(qName, myContext);
                }

                if (clazz instanceof JSClass jsClass) {
                    if (!wasSet) {
                        myAddOnlyCompleteMatches = true;
                    }

                    final JSAttributeList attributeList = jsClass.getAttributeList();
                    if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)
                        && !OBJECT_CLASS_NAME.equals(qName)) {
                        myAddOnlyCompleteMatches = false;
                    }
                }
            }
        }
    }

    private static boolean isObjectSourceThatDoesNotGiveExactKnowledgeAboutFunctionType(final Object source) {
        return source instanceof JSFunctionExpression;
    }

    @RequiredReadAction
    private void updateCanUseOnlyCompleteMatches(final JSClass jsClass) {
        final JSAttributeList attributeList = jsClass != null ? jsClass.getAttributeList() : null;
        if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
            myAddOnlyCompleteMatches = false;
        }
    }

    @RequiredReadAction
    public void addLocalResults(final List<PsiElement> results) {
        if (results == null) {
            return;
        }

        final Set<String> processedCandidateNames = new HashSet<>(results.size());

        for (PsiElement e : results) {
            if (e instanceof PsiNamedElement namedElement) {
                String name = ResolveProcessor.getName(namedElement);
                String qName = namedElement instanceof JSQualifiedNamedElement qualifiedNamedElement
                    ? qualifiedNamedElement.getQualifiedName()
                    : name;

                if (processedCandidateNames.contains(qName)) {
                    continue;
                }
                processedCandidateNames.add(qName);

                addCompleteMatch(namedElement, name, false);
            }
        }
    }

    class CompletionTypeProcessor implements TypeProcessor {
        final List<String[]> possibleNameIds;
        private PsiElement myUnknownElement;

        CompletionTypeProcessor(List<String[]> _possibleNameIds) {
            possibleNameIds = _possibleNameIds;
        }

        @Override
        @RequiredReadAction
        public void process(@Nonnull String type, EvaluateContext context, PsiElement source) {
            if (context.visitedTypes.contains(type)) {
                return;
            }
            context.visitedTypes.add(type);
            if (JSTypeEvaluateManager.isArrayType(type)) {
                type = ARRAY_TYPE_NAME;
            }
            updateCanUseOnlyCompleteMatchesFromString(type, source, null);
            type = buildIndexListFromQNameAndCorrectQName(type, source, possibleNameIds);
            addSupers(type, possibleNameIds, context);
        }

        @Override
        public Set<JavaScriptFeature> getFeatures() {
            return myFeatures;
        }

        @Override
        public boolean ecma() {
            return ecmal4;
        }

        @Override
        public void setUnknownElement(@Nonnull PsiElement element) {
            myUnknownElement = element;
        }

        public boolean getAllTypesResolved() {
            return myUnknownElement == null;
        }
    }

    private void addSupers(final String type, final List<String[]> possibleNameIds, final EvaluateContext context) {
        final String iteratedType = myIteratedTypeName;
        myIteratedTypeName = type;
        doIterateHierarchy(
            type,
            clazz -> {
                String qname = clazz.getQualifiedName();
                if (!context.visitedTypes.contains(qname)) {
                    context.visitedTypes.add(qname);
                    updateCanUseOnlyCompleteMatchesFromString(qname, clazz, clazz);
                    buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameIds);
                }
                return true;
            }
        );

        myIteratedTypeName = iteratedType;
    }

    public Object[] getResult() {
        List<Object> results = new ArrayList<>();
        for (Object o : myNamesList) {
            results.add(o);
        }

        for (Object o : myPartialMatchNamesMapFromSameFile.values()) {
            results.add(o);
        }

        for (Object o : myPartialMatchNamesMap.values()) {
            results.add(o);
        }

        return ArrayUtil.toObjectArray(results);
    }

    @Override
    @RequiredReadAction
    public boolean execute(@Nonnull PsiElement element, ResolveState state) {
        if (element instanceof JSNamedElement namedElement) {
            addCompleteMatch(namedElement, namedElement.getName());
        }

        return true;
    }

    private void doAdd(final String nameId, final PsiElement element) {
        final String name = nameId != null ? nameId : null;

        boolean seemsToBePrivateSymbol =
            name != null && name.length() > 0 && name.charAt(0) == '_' && name.length() > 1 && name.charAt(1) != '_';
        boolean privateSymbol = seemsToBePrivateSymbol;

        if (seemsToBePrivateSymbol) {
            if (!(myTargetFile instanceof JSFile)) {
                // no interest in html, jsp for private symbols
                return;
            }

            if (myContextNameIds == null || myContextNameIds.length == 0) {
                privateSymbol = false;
            }
        }


        MatchType matchType = MatchType.COMPLETE;

        if (matchType == MatchType.COMPLETE && !ecmal4 && myProcessOnlyTypes) {
            if (((name != null && name.length() > 0 && Character.isLowerCase(name.charAt(0))) || seemsToBePrivateSymbol)) {
                matchType = MatchType.PARTIAL;
            }
        }

        if (matchType == MatchType.PARTIAL || (privateSymbol && matchType == MatchType.COMPLETE)) {
            addPartialMatch(element, nameId);

        }
        else if (matchType == MatchType.COMPLETE) {
            addCompleteMatch(element, nameId);
        }
    }

    @RequiredReadAction
    private LookupElement addLookupValue(PsiElement element, final String name, JSLookupUtil.LookupPriority priority) {
        return JSLookupUtil.createLookupItem(element, name, priority);
    }

    @Override
    @RequiredReadAction
    protected String[] calculateContextIds(final JSReferenceExpression jsReferenceExpression) {
        return JSResolveUtil.buildNameIdsForQualifier(JSResolveUtil.getRealRefExprQualifier(jsReferenceExpression));
    }

    @Override
    protected boolean isFromRelevantFileOrDirectory() {
        return super.isFromRelevantFileOrDirectory(); // || myTargetFiles.contains(myCurrentFile);
    }

    private void addCompleteMatch(final PsiElement _element, String nameId) {
        addCompleteMatch(_element, nameId, true);
    }

    private void addCompleteMatch(final PsiElement _element, String nameId, boolean doFilterting) {
        if (!doAdd(_element, nameId, doFilterting)) {
            boolean removedFromPartialNames = false;
            final Object el = myNames2CandidatesMap.get(nameId);

            if (el != null) {
                removedFromPartialNames =
                    myPartialMatchNamesMapFromSameFile.remove(nameId) != null || myPartialMatchNamesMap.remove(nameId) != null;
            }
            if (!removedFromPartialNames) {
                return;
            }
        }

        PsiElement element = _element;

        if (isFromRelevantFileOrDirectory() && !myAddOnlyCompleteMatches) {
            final Object o = addLookupValue(element, nameId, JSLookupUtil.LookupPriority.HIGHEST);
            if (o != null) {
                myNamesList.add(myThisFileNameListCount++, o);
            }
            else {
                myNames2CandidatesMap.remove(nameId);
            }
        }
        else {
            final Object o = addLookupValue(element, nameId, JSLookupUtil.LookupPriority.HIGH);
            if (o != null) {
                myNamesList.add(o);
            }
            else {
                myNames2CandidatesMap.remove(nameId);
            }
        }
    }

    private boolean doAdd(Object element, String nameId, boolean doFilterting) {
        if (nameId == null || (doFilterting && myNames2CandidatesMap.get(nameId) != null)) {
            return false;
        }
        myNames2CandidatesMap.put(nameId, element);
        return true;
    }

    private void addPartialMatch(final PsiElement _element, String nameId) {
        if (myAddOnlyCompleteMatches) {
            return;
        }
        if (!doAdd(_element, nameId, true)) {
            return;
        }

        PsiElement element = _element;

        final Map<String, Object> targetNamesMap;
        final JSLookupUtil.LookupPriority priority;

        if (isFromRelevantFileOrDirectory()) {
            priority = hasSomeSmartnessAvailable ? JSLookupUtil.LookupPriority.HIGHER : JSLookupUtil.LookupPriority.HIGHEST;
            targetNamesMap = myPartialMatchNamesMapFromSameFile;
        }
        else {
            priority = hasSomeSmartnessAvailable ? JSLookupUtil.LookupPriority.NORMAL : JSLookupUtil.LookupPriority.HIGH;

            targetNamesMap = myPartialMatchNamesMap;
        }

        final Object o = addLookupValue(element, nameId, priority);

        if (o != null) {
            targetNamesMap.put(nameId, o);
        }
        else {
            myNames2CandidatesMap.remove(nameId);
        }
    }
}
