/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

import gnu.trove.THashSet;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.lang.javascript.validation.JSUnusedImportsHelper;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.UserDataHolderBase;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlToken;
import com.intellij.util.ArrayUtil;
import com.intellij.util.SmartList;

/**
 * @by Maxim.Mossienko
*/
public class ResolveProcessor extends UserDataHolderBase implements PsiScopeProcessor {
  private static final Key<String> ASKING_FOR_QUALIFIED_IMPORT = Key.create("asking.for.import.of.qname");
  protected static final Key<Boolean> LOOKING_FOR_USE_NAMESPACES = Key.create("looking.for.use.directive");

  private final Set<String> visitedClasses = new THashSet<String>();

  private boolean myToStopOnAssignment;
  protected final String myName;
  private PsiElement myResult;
  private PsiElement myCandidateResult;
  private List<PsiElement> myResults;
  private List<JSImportStatement> myImportsUsed;
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

  public static final Key<JSImportStatement> IMPORT_KEY = Key.create("import.key");
  private boolean myClassDeclarationStarted;
  private PsiElement placeTopParent;
  protected boolean ecma;
  public static final String AS3_NAMESPACE = "AS3";

  public ResolveProcessor(final String name) {
    this(name, false);
  }

  public ResolveProcessor(final String name, boolean toStopOnAssignment) {
    myName = name;
    myToStopOnAssignment = toStopOnAssignment;
    place = null;
  }

  public ResolveProcessor(final String name, PsiElement _place) {
    myName = name;
    place = _place;
    final boolean b = place instanceof JSReferenceExpression && ((JSReferenceExpression)place).getQualifier() == null;
    allowUnqualifiedStaticsFromInstance = b;

    if (myName != null && place instanceof JSReferenceExpression) {
      final ASTNode node = place.getNode().findChildByType(JSTokenTypes.COLON_COLON);
      String explicitNs = null;

      // TODO: e.g. protected is also ns
      if (node != null) {
        final JSExpression expression = ((JSReferenceExpression)place).getQualifier();
        if (expression instanceof JSReferenceExpression) {
          anyNsAllowed = ((JSReferenceExpression)expression).resolve() instanceof JSVariable;
        }

        explicitNs = expression != null ? expression.getText() : null;
      } else {
        final JSExpression qualifier = ((JSReferenceExpression)place).getQualifier();

        if (qualifier instanceof JSReferenceExpression &&
            qualifier.getNode().findChildByType(JSTokenTypes.COLON_COLON) != null
            ) {
          anyNsAllowed = ((JSReferenceExpression)qualifier).resolve() instanceof JSVariable;
          explicitNs = ((JSReferenceExpression)qualifier).getReferencedName();
        }
      }

      if (explicitNs != null && !anyNsAllowed) {
        openedNses = new THashSet<String>();
        openedNses.add(explicitNs);
        defaultNsIsNotAllowed = true;
      }
    }

    if (myName == null && place instanceof JSReferenceExpression) {
      PsiElement parent = place.getParent();

      if (parent instanceof JSReferenceList) {
        if (parent.getNode().getElementType() == JSElementTypes.EXTENDS_LIST) {
          final PsiElement element = parent.getParent();
          if (element instanceof JSClass) {
            if (((JSClass)element).isInterface()) {
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

  public JSImportStatement getImportUsed() {
    return myImportsUsed != null ? myImportsUsed.get(0):null;
  }

  public List<PsiElement> getResults() {
    if (myResults == null && myResult == null && myCandidateResult != null) {
      myResults = new ArrayList<PsiElement>(1);
      myResults.add(myCandidateResult);
    }
    return myResults;
  }

  public boolean execute(PsiElement element, ResolveState state) {
    if ((element instanceof JSVariable && !(element instanceof JSParameter)) || element instanceof JSFunction) {
      final JSAttributeList attributeList = ((JSAttributeListOwner)element).getAttributeList();

      // TODO: we should accept such values during resolve but make them invalid
      if (!acceptPrivateMembers) {
        if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE) {
          return true;
        }
      }

      if (!acceptProtectedMembers) {
        if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PROTECTED) {
          return true;
        }
      }

      if (myClassDeclarationStarted) {
        if (processStatics) {
          if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) return true;
        } else if (!allowUnqualifiedStaticsFromInstance) {
          if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) return true;
        }
      }

      String attributeNs = attributeList != null ? attributeList.getNamespace() : null;

      if (openedNses == null && attributeNs != null) {
        if (!anyNsAllowed &&
            place instanceof JSReferenceExpression &&
            !JSResolveUtil.isExprInTypeContext((JSReferenceExpression)place)) {
          openedNses = JSResolveUtil.calculateOpenNses(place);
        }
      }

      if (openedNses != null && !openedNses.contains(attributeNs) && (!AS3_NAMESPACE.equals(attributeNs) || !ecma) // AS3 is opened by default from compiler settings
         ) {
        if (attributeNs != null || defaultNsIsNotAllowed) return true;
      }
    }

    if (myAcceptOnlyClasses || myAcceptOnlyInterfaces) {
      if (element instanceof JSClass) {
        final boolean isInterface = ((JSClass)element).isInterface();
        
        if ((myAcceptOnlyClasses && isInterface) ||
            (myAcceptOnlyInterfaces && !isInterface)
           ) {
          return true;
        }
      }
    }

    if (place != null && element instanceof JSFunction && !(element instanceof JSFunctionExpression)) {
      final JSFunction function = (JSFunction)element;
      final PsiElement placeParent = place.getParent();

      if (placeParent instanceof JSDefinitionExpression ||
          (myName == null && placeParent instanceof JSExpressionStatement /* when complete of setter*/) ||
          (place instanceof JSFunction && ((JSFunction)place).isSetProperty())
          ) {
        PsiElement clazz;
        if (function.isGetProperty() &&
              (myName !=  null || !(placeParent instanceof JSExpressionStatement) || !((clazz = function.getParent()) instanceof JSClass) ||
               ((JSClass)clazz).findFunctionByNameAndKind(function.getName(), JSFunction.FunctionKind.SETTER) != null
              )
            ) {
          return true;
        }
      } else {
        if (function.isSetProperty()) return true;
      }
    }

    if (place != null && (place.getParent() instanceof JSNewExpression || place instanceof JSSuperExpression)) {
      if(element instanceof JSClass) {
        final JSFunction byName = ((JSClass)element).findFunctionByName(((JSClass)element).getName());
        if (byName != null) element = byName;
      } else if (element instanceof JSFunction &&
                 element.getParent() instanceof JSClass /*TODO: in mxml?*/&&
                 !((JSFunction)element).isConstructor() &&
                 !((JSFunction)element).isGetProperty() 
                ) {
        return true;
      }
    } else if (element instanceof JSFunction && ((JSFunction)element).isConstructor() && !(place instanceof JSClass)) {
      return true;
    }

    if (placeTopParent == null && place != null) {
      placeTopParent = JSResolveUtil.getTopReferenceExpression(place);
    }
    if (placeTopParent instanceof JSReferenceExpression &&
        JSResolveUtil.isExprInStrictTypeContext((JSReferenceExpression)placeTopParent) &&
        place != placeTopParent) {
      if (element instanceof JSVariable || element instanceof JSFunction) return true;
    }

    boolean checkStopOnMatch = false;
    boolean doStopOnMatch = false;

    PsiElement elementToCheck = element;
    if (elementToCheck instanceof JSFunctionExpression) {
      final PsiElement parent = elementToCheck.getParent();
      if (parent instanceof JSAssignmentExpression) {
        elementToCheck = ((JSAssignmentExpression)parent).getLOperand();
      }
    }

    if (elementToCheck instanceof JSDefinitionExpression) {
      boolean toProcess = false;
      final JSExpression expression = ((JSDefinitionExpression)elementToCheck).getExpression();
      
      if (expression instanceof JSReferenceExpression &&
          ((JSReferenceExpression)expression).getQualifier() == null
        ) {
        toProcess = true;
      }

      if (!toProcess) return true;
      checkStopOnMatch = true;
      doStopOnMatch = myToStopOnAssignment;
    }

    if (element instanceof PsiNamedElement) {
      if (myName == null || myName.equals(getName(element))) {
        element = getElement(element);
        if (checkStopOnMatch && !doStopOnMatch) {
          myCandidateResult = element;
        } else {
          if (myName != null) {
            JSImportStatement s = state != null ? state.get(IMPORT_KEY):null;
            int previousResultsSize = myResults != null ? myResults.size():0;

            s = checkQualifiedNameHasNecessaryImport(element, s, previousResultsSize);

            if (s != null || myImportsUsed != null) {
              if (myImportsUsed == null) {
                myImportsUsed = new SmartList<JSImportStatement>();
                for(int i = 0; i < previousResultsSize; ++i) myImportsUsed.add(null);
              }

              myImportsUsed.add(s);
            }
          }
          if (myResults == null) myResults = new ArrayList<PsiElement>(1);
          myResults.add(element);
          myResult = element;
          return myName == null;
        }
      }
    }

    return true;
  }

  private JSImportStatement checkQualifiedNameHasNecessaryImport(PsiElement element, JSImportStatement s, int previousResultsSize) {
    if (s == null && (element instanceof JSClass || element instanceof JSFunction || element instanceof JSVariable ||
                      element instanceof JSNamespaceDeclaration)) {

      if (placeTopParent instanceof JSReferenceExpression && !(placeTopParent.getParent() instanceof JSImportStatement)) {
        final String qName = ((JSQualifiedNamedElement)element).getQualifiedName();

        if (qName != null && qName.indexOf('.') != -1) {
          final ResolveProcessor processor = new ResolveProcessor(myName) {
            @Override
            public boolean execute(PsiElement element, ResolveState state) {
              if (element instanceof JSQualifiedNamedElement) {
                if (!qName.equals(((JSQualifiedNamedElement)element).getQualifiedName())) return true;
              } else {
                return true;
              }
              return super.execute(element, state);
            }
          };

          if (!JSUnusedImportsHelper.isSomeNodeThatShouldNotHaveImportsWhenQualified((JSReferenceExpression)placeTopParent, element)) {
            processor.putUserData(ASKING_FOR_QUALIFIED_IMPORT, qName);
            JSResolveUtil.treeWalkUp(processor, placeTopParent, placeTopParent, place);
            boolean noImportNoResolve = processor.getResult() == null;

            if (noImportNoResolve) {
              if (myResolveStatus == null) {
                myResolveStatus = new SmartList<Boolean>();
                for(int i = 0; i < previousResultsSize; ++i) myResolveStatus.add(Boolean.TRUE);
              }

              myResolveStatus.add(Boolean.FALSE);
            } else {
              final ResolveResult[] resultsAsResolveResults = processor.getResultsAsResolveResults();
              if (resultsAsResolveResults.length != 0) s = ((JSResolveUtil.MyResolveResult)resultsAsResolveResults[0]).getImportUsed();
            }
          }
        }
      }
    }
    return s;
  }

  private static PsiElement getElement(final PsiElement element) {
    if (element instanceof XmlTag) {
      return ((XmlTag)element).getAttribute("name").getValueElement().getChildren()[1];
    }
    return element;
  }

  public static String getName(final PsiElement element) {
    if (element instanceof JSNamedElement) return ((JSNamedElement)element).getName();
    if (element instanceof XmlTag) return ((XmlTag)element).getAttributeValue("name");
    if (element instanceof XmlToken) return element.getText();
    return null;
  }

  public <T> T getHint(Key<T> hintClass) {
    return null;
  }

  protected void setTypeName(final String qualifiedName) {
    myTypeName = qualifiedName;
  }

  public void configureClassScope(final JSClass jsClass) {
    if (jsClass != null) myClassScopeTypeName = jsClass.getQualifiedName();
    else {
      acceptProtectedMembers = false;
      acceptProtectedMembersSet = true;
    }
  }

  public void handleEvent(Event event, Object associated) {
    if (event == Event.SET_DECLARATION_HOLDER) {
      if (associated instanceof JSClass) {
        myClassDeclarationStarted = true;
        final JSClass jsClass = (JSClass)associated;
        final String qName = jsClass.getQualifiedName();

        if (!encounteredDynamicClassesSet) {
          final JSAttributeList attributeList = jsClass.getAttributeList();
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
              final PsiElement element = JSClassImpl.findClassFromNamespace(myClassScopeTypeName, place);
              if (element instanceof JSClass) {
                final boolean b = element.processDeclarations(new ResolveProcessor(null) {
                  {
                    setTypeContext(true);
                    setToProcessMembers(false);
                    setToProcessHierarchy(true);
                    acceptProtectedMembers = false;
                    setLocalResolve(true);
                  }

                  @Override
                  public boolean execute(final PsiElement element, final ResolveState state) {
                    if (!(element instanceof JSClass)) return true;
                    String classQName = ((JSClass)element).getQualifiedName();
                    return qName == classQName || (qName != null && !qName.equals(classQName));
                  }
                }, ResolveState.initial(), element, element);

                acceptProtectedMembers = !b;
              }
            }
          }
        }

        if (processStatics) {
          if (myTypeName != null && !myTypeName.equals(jsClass.getQualifiedName()) ||
              myTypeName == null && myClassScopeTypeName != null && !myClassScopeTypeName.equals(jsClass.getQualifiedName())
             ) {
            processStatics = false;
          }
        }
      } else if (associated instanceof JSAttributeListOwner) {
        final JSAttributeList attributeList = ((JSAttributeListOwner)associated).getAttributeList();

        if (attributeList != null &&
            attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) &&
            ( place == null ||
              !(place.getParent() instanceof JSNewExpression)
            )
          ) {
          processStatics = true;
        }

        if (associated instanceof JSFunctionExpression) {
          final PsiElement context = ((PsiElement)associated).getContainingFile().getContext();
          if (!(context instanceof XmlAttributeValue)) encounteredFunctionExpression = true; // we create anonymous fun exprs for event handlers
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

  public void setToProcessHierarchy(final boolean toProcessHierarchy) {
    this.toProcessHierarchy = toProcessHierarchy;
  }

  public boolean isToSkipClassDeclarationOnce() {
    return toSkipClassDeclarationOnce;
  }

  public void setToSkipClassDeclarationsOnce(final boolean toSkipClassDeclarationOnce) {
    this.toSkipClassDeclarationOnce = toSkipClassDeclarationOnce;
  }

  public boolean foundAllValidResults() {
    return !encounteredFunctionExpression; // TODO: with statement too?
  }

  public void setTypeContext(final boolean b) {
    myTypeContext = b;
  }

  public boolean isTypeContext() {
    return myTypeContext;
  }

  public ResolveResult[] getResultsAsResolveResults() {
    final List<PsiElement> processorResults = getResults();
    if (processorResults == null) return ResolveResult.EMPTY_ARRAY;
    final ResolveResult[] results = new ResolveResult[processorResults.size()];

    for(int i = 0; i < results.length; ++i) {
      final PsiElement element = processorResults.get(i);
      results[i] = new JSResolveUtil.MyResolveResult(
        element,
        myImportsUsed != null ? myImportsUsed.get(i):null,
        myResolveStatus != null ? myResolveStatus.get(i):true
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

  public Object[] getResultsAsObjects(String qualifiedNameToSkip) {
    final List<PsiElement> processorResults = getResults();
    if (processorResults == null) return ArrayUtil.EMPTY_OBJECT_ARRAY;
    final int numberOfVariants = processorResults.size();
    final List<Object> objects = new ArrayList<Object>(numberOfVariants);
    final Set<String> processedCandidateNames = new THashSet<String>(numberOfVariants);

    for(int i = 0; i < numberOfVariants; ++i) {
      final PsiElement namedElement = processorResults.get(i);
      final String name = getName(namedElement);

      String qName = namedElement instanceof JSQualifiedNamedElement ? ((JSQualifiedNamedElement)namedElement).getQualifiedName():name;

      if (processedCandidateNames.contains(qName)) continue;
      processedCandidateNames.add(qName);

      if (qualifiedNameToSkip != null && qualifiedNameToSkip.equals(qName)) {
        continue;
      }

      objects.add(JSLookupUtil.createPrioritizedLookupItem(namedElement, name, 3));
    }
    return ArrayUtil.toObjectArray(objects);
  }

  public boolean isToProcessMembers() {
    return toProcessMembers;
  }

  public void setToProcessMembers(final boolean toProcessMembers) {
    this.toProcessMembers = toProcessMembers;
  }

  public boolean checkVisited(String className) {
    if (visitedClasses.contains(className)) return true;
    visitedClasses.add(className);
    return false;
  }

  public void setProcessStatics(final boolean processStatics) {
    this.processStatics = processStatics;
  }

  public boolean isLocalResolve() {
    return localResolve;
  }

  public void setLocalResolve(final boolean localResolve) {
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
