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

import gnu.trove.THashSet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.extapi.psi.PsiElementBase;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;

/**
 * @by Maxim.Mossienko
 */
public class VariantsProcessor extends BaseJSSymbolProcessor
{
	private Map<String, Object> myNames2CandidatesMap = new HashMap<String, Object>();
	private int myThisFileNameListCount;
	private List<Object> myNamesList = new ArrayList<Object>();

	private Map<String, Object> myPartialMatchNamesMap = new HashMap<String, Object>();
	private Map<String, Object> myPartialMatchNamesMapFromSameFile = new HashMap<String, Object>();
	//private Set<PsiFile> myTargetFiles;
	private final boolean hasSomeSmartnessAvailable;
	private boolean hasSomeInfoAvailable;
	private final String[][] myNameIdsArray;
	private boolean myProcessOnlyProperties, myProcessOnlyTypes;
	private boolean myProcessOnlyClasses, myProcessOnlyInterfaces;

	private boolean myAddOnlyCompleteMatchesSet;

	@NonNls
	private static final String INT_TYPE = "int";
	@NonNls
	private static final String UINT_TYPE = "uint";
	@NonNls
	private static final String VOID_TYPE = "void";
	@NonNls
	private static final String ANY_TYPE = "*";
	private boolean myInVarDcl;
	private boolean myInFuncDcl;
	private boolean myStrictTypeContext;
	@NonNls
	private static final String OBJECT_CLASS_NAME = "Object";

	public VariantsProcessor(String[] nameIds, PsiFile targetFile, boolean skipDclsInTargetFile, PsiElement context)
	{
		super(targetFile.getOriginalFile() != null ? targetFile.getOriginalFile() : targetFile, skipDclsInTargetFile, context, nameIds);
		nameIds = myContextNameIds;

		myProcessOnlyProperties = context instanceof JSProperty;
		hasSomeInfoAvailable = nameIds != null && nameIds.length > 0;
		final List<String[]> possibleNameComponents = new ArrayList<String[]>(1);
		myCurrentFile = targetFile.getOriginalFile();
		//myTargetFiles = new HashSet<PsiFile>();

		boolean allTypesResolved = false;
		boolean doAddContextIds = true;

		final JSClass jsClass = PsiTreeUtil.getParentOfType(context, JSClass.class);

		if(context instanceof JSReferenceExpression)
		{
			final JSReferenceExpression refExpr = (JSReferenceExpression) context;
			JSExpression qualifier = refExpr.getQualifier();

			if(qualifier != null)
			{
				if(qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression)
				{
					if(jsClass != null)
					{
						myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
						updateCanUseOnlyCompleteMatches(jsClass);
						doAddContextIds = false;
					}
					else if(JSResolveUtil.getTypeFromTagNameInMxml(myCurrentFile.getContext()) != null)
					{
						myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
					}
				}

				myIndex.getDefaultPackage(); // ensure index is up to date
				final CompletionTypeProcessor processor = new CompletionTypeProcessor(possibleNameComponents);
				doEvalForExpr(getOriginalQualifier(qualifier), myTargetFile, processor);
				allTypesResolved = processor.getAllTypesResolved();
			}
			else
			{
				final PsiElement parent = refExpr.getParent();
				boolean strictTypeContext;

				if(parent instanceof JSImportStatement)
				{
					myProcessOnlyTypes = true;
				}
				else if((strictTypeContext = JSResolveUtil.isExprInTypeContext(refExpr)) || (ecmal4 && JSResolveUtil.isInPlaceWhereTypeCanBeDuringCompletion(refExpr)))
				{
					myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
					myProcessOnlyTypes = true;
					myStrictTypeContext = strictTypeContext;
					myInFuncDcl = parent instanceof JSFunction;
					myInVarDcl = parent instanceof JSVariable;
					allTypesResolved = true;
					addPackageScope(possibleNameComponents, jsClass, refExpr);

					if(parent instanceof JSReferenceList)
					{
						if(parent.getNode().getElementType() == JSElementTypes.EXTENDS_LIST)
						{
							final PsiElement element = parent.getParent();
							if(element instanceof JSClass)
							{
								if(((JSClass) element).isInterface())
								{
									myProcessOnlyClasses = false;
									myProcessOnlyInterfaces = true;
								}
								else
								{
									myProcessOnlyClasses = true;
									myProcessOnlyInterfaces = false;
								}
							}
						}
						else
						{
							myProcessOnlyClasses = false;
							myProcessOnlyInterfaces = true;
						}
					}
				}
				else if(jsClass != null)
				{
					myAddOnlyCompleteMatches = myAddOnlyCompleteMatchesSet = true;
					updateCanUseOnlyCompleteMatches(jsClass);
				}
			}
		}

		if(hasSomeInfoAvailable && (!allTypesResolved || (!possibleNameComponents.isEmpty() &&
				possibleNameComponents.get(0).length == 1 &&
				"Function".equals(possibleNameComponents.get(0)[0]))))
		{
			if(doAddContextIds)
			{
				possibleNameComponents.add(nameIds);
			}

			doIterateTypeHierarchy(nameIds, new HierarchyProcessor()
			{
				@Override
				public boolean processNamespace(final JSNamespace ns)
				{
					updateCanUseOnlyCompleteMatchesFromNs(ns);
					possibleNameComponents.add(ns.getIndices());
					return true;
				}

				@Override
				public boolean processClass(final JSClass clazz)
				{
					updateCanUseOnlyCompleteMatchesFromString(clazz.getQualifiedName(), clazz, clazz);
					buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameComponents);
					return true;
				}
			});
		}

		myNameIdsArray = new String[possibleNameComponents.size()][];
		possibleNameComponents.toArray(myNameIdsArray);

		hasSomeSmartnessAvailable = myNameIdsArray != null && myNameIdsArray.length > 0;
	}

	private void updateCanUseOnlyCompleteMatchesFromNs(final JSNamespace ns)
	{
		updateCanUseOnlyCompleteMatchesFromString(ns.getQualifiedName(myIndex), ns, null);
	}

	private void updateCanUseOnlyCompleteMatchesFromString(final String qName, Object source, PsiElement clazz)
	{
		final boolean wasSet = myAddOnlyCompleteMatchesSet;

		if(myAddOnlyCompleteMatches || !wasSet)
		{
			myAddOnlyCompleteMatchesSet = true;
			if(qName.equals("*") ||
					(qName.equals(OBJECT_CLASS_NAME) && (myIteratedTypeName == null ||
							qName.equals(myIteratedTypeName) ||
							// skip adding complete matches when context name is qualifier
							(myContextNameIds != null &&
									myContextNameIds.length > 0 &&
									myIteratedTypeName.equals(myContextNameIds[myContextNameIds.length - 1]) &&
									myContext instanceof JSReferenceExpression &&
									((JSReferenceExpression) myContext).getQualifier() instanceof JSReferenceExpression))) /* something explicitly marked as Object*/ ||
					(qName.equals(FUNCTION_TYPE_NAME) && isObjectSourceThatDoesNotGiveExactKnowledgeAboutFunctionType(source)))
			{
				myAddOnlyCompleteMatches = false;
			}
			else
			{
				if(clazz == null)
				{
					clazz = JSResolveUtil.findClassByQName(qName, myContext);
				}

				if(clazz instanceof JSNamedElementProxy)
				{
					final JSNamedElementProxy elementProxy = (JSNamedElementProxy) clazz;
					final JSNamedElementProxy.NamedItemType itemType = elementProxy.getType();

					if(!wasSet && (itemType == JSNamedElementProxy.NamedItemType.Clazz))
					{
						myAddOnlyCompleteMatches = true;
					}

					if(itemType == JSNamedElementProxy.NamedItemType.Clazz)
					{
						if(elementProxy.hasProperty(JSNamedElementProxy.Property.Dynamic) && !OBJECT_CLASS_NAME.equals(qName))
						{
							myAddOnlyCompleteMatches = false;
						}
					}
				}
				else if(clazz instanceof JSClass)
				{
					if(!wasSet)
					{
						myAddOnlyCompleteMatches = true;
					}

					final JSAttributeList attributeList = ((JSClass) clazz).getAttributeList();
					if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC) && !OBJECT_CLASS_NAME.equals(qName))
					{
						myAddOnlyCompleteMatches = false;
					}
				}
			}
		}
	}

	private static boolean isObjectSourceThatDoesNotGiveExactKnowledgeAboutFunctionType(final Object source)
	{
		return source instanceof JSFunctionExpression || source instanceof JSNamedElementProxy && ((JSNamedElementProxy) source).getType() != JSNamedElementProxy.NamedItemType.Function;
	}

	private void updateCanUseOnlyCompleteMatches(final JSClass jsClass)
	{
		final JSAttributeList attributeList = jsClass != null ? jsClass.getAttributeList() : null;
		if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC))
		{
			myAddOnlyCompleteMatches = false;
		}
	}

	public void addLocalResults(final List<PsiElement> results)
	{
		if(results == null)
		{
			return;
		}

		final Set<String> processedCandidateNames = new THashSet<String>(results.size());

		for(PsiElement e : results)
		{
			if(e instanceof PsiNamedElement)
			{
				final PsiNamedElement namedElement = (PsiNamedElement) e;

				String name = ResolveProcessor.getName(namedElement);
				String qName = namedElement instanceof JSQualifiedNamedElement ? ((JSQualifiedNamedElement) namedElement).getQualifiedName() : name;

				if(processedCandidateNames.contains(qName))
				{
					continue;
				}
				processedCandidateNames.add(qName);

				addCompleteMatch(namedElement, name, false);
			}
		}
	}

	class CompletionTypeProcessor implements TypeProcessor
	{
		final List<String[]> possibleNameIds;
		private PsiElement myUnknownElement;

		CompletionTypeProcessor(List<String[]> _possibleNameIds)
		{
			possibleNameIds = _possibleNameIds;
		}

		@Override
		public void process(String type, final EvaluateContext context, final PsiElement source)
		{
			if(context.visitedTypes.contains(type))
			{
				return;
			}
			context.visitedTypes.add(type);
			if(context.typeEvaluateManager.isArrayType(type))
			{
				type = ARRAY_TYPE_NAME;
			}
			updateCanUseOnlyCompleteMatchesFromString(type, source, null);
			type = buildIndexListFromQNameAndCorrectQName(type, source, possibleNameIds);
			addSupers(type, possibleNameIds, context);
		}

		@Override
		public boolean ecma()
		{
			return ecmal4;
		}

		@Override
		public void setUnknownElement(PsiElement element)
		{
			myUnknownElement = element;
		}

		public boolean getAllTypesResolved()
		{
			return myUnknownElement == null;
		}
	}

	private void addSupers(final String type, final List<String[]> possibleNameIds, final EvaluateContext context)
	{
		final String iteratedType = myIteratedTypeName;
		myIteratedTypeName = type;
		doIterateHierarchy(type, new HierarchyProcessor()
		{
			@Override
			public boolean processNamespace(final JSNamespace ns)
			{
				final String qname = ns.getQualifiedName(myIndex);
				if(!context.visitedTypes.contains(qname))
				{
					context.visitedTypes.add(qname);
					updateCanUseOnlyCompleteMatchesFromString(qname, ns, null);
					possibleNameIds.add(ns.getIndices());
				}
				return true;
			}

			@Override
			public boolean processClass(final JSClass clazz)
			{
				String qname = clazz.getQualifiedName();
				if(!context.visitedTypes.contains(qname))
				{
					context.visitedTypes.add(qname);
					updateCanUseOnlyCompleteMatchesFromString(qname, clazz, clazz);
					buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameIds);
				}
				return true;
			}
		});

		myIteratedTypeName = iteratedType;
	}

	public Object[] getResult()
	{
		if(myProcessOnlyTypes && ecmal4 && (myInFuncDcl || myInVarDcl))
		{
			addCompleteMatch(INT_TYPE, INT_TYPE);
			addCompleteMatch(UINT_TYPE, UINT_TYPE);
			if(myInFuncDcl)
			{
				addCompleteMatch(VOID_TYPE, VOID_TYPE);
			}
			addCompleteMatch(ANY_TYPE, ANY_TYPE);
		}

		List<Object> results = new ArrayList<Object>();
		for(Object o : myNamesList)
		{
			results.add(o);
		}

		for(Object o : myPartialMatchNamesMapFromSameFile.values())
		{
			results.add(o);
		}

		for(Object o : myPartialMatchNamesMap.values())
		{
			results.add(o);
		}

		return ArrayUtil.toObjectArray(results);
	}

	@Override
	public boolean execute(PsiElement element, ResolveState state)
	{
		if(element instanceof JSNamedElement)
		{
			final JSNamedElement namedElement = (JSNamedElement) element;

			addCompleteMatch(namedElement, namedElement.getName());
		}

		return true;
	}

	@Override
	public boolean processFunction(JSNamespace namespace, final String nameId, JSNamedElement function)
	{
		if(myCurrentFile != myTargetFile ||
				myDefinitelyGlobalReference ||
				((function instanceof JSNamedElementProxy && (((JSNamedElementProxy) function).getType() != JSNamedElementProxy.NamedItemType.Function || !isGlobalNS(namespace))) || function
						instanceof JSFunctionExpression))
		{
			doAdd(namespace, nameId, function);
		}
		return true;
	}

	@Override
	public boolean processClass(final JSNamespace namespace, final String nameId, final JSNamedElement clazz)
	{
		doAdd(namespace, nameId, clazz);
		return true;
	}

	@Override
	public boolean processProperty(JSNamespace namespace, final String nameId, JSNamedElement property)
	{
		doAdd(namespace, nameId, property);
		return true;
	}

	@Override
	public boolean processDefinition(final JSNamespace namespace, final String nameId, final JSNamedElement refExpr)
	{
		doAdd(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processNamespace(final JSNamespace namespace, final String nameId, final JSNamedElement refExpr)
	{
		doAdd(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitNamespace(final JSNamespace namespace, final String nameId, final PsiElement refExpr, boolean finalReference)
	{
		doAdd(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitFunction(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
	{
		doAdd(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitVariable(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
	{
		doAdd(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public String getRequiredNameId()
	{
		return null;
	}

	@Override
	public boolean processTag(JSNamespace namespace, final String nameId, PsiNamedElement namedElement, final String attrName)
	{
		doAdd(namespace, nameId, namedElement);
		return true;
	}

	@Override
	public boolean processVariable(JSNamespace namespace, final String nameId, JSNamedElement variable)
	{
		if(myCurrentFile != myTargetFile ||
				myDefinitelyGlobalReference ||
				(variable instanceof JSNamedElementProxy && ((JSNamedElementProxy) variable).getType() == JSNamedElementProxy.NamedItemType.MemberVariable))
		{
			doAdd(namespace, nameId, variable);
		}
		return true;
	}

	@Override
	public PsiFile getBaseFile()
	{
		return myTargetFile;
	}

	private void doAdd(JSNamespace namespace, final String nameId, final PsiElement element)
	{
		final String name = nameId != null ? nameId : null;

		boolean seemsToBePrivateSymbol = name != null && name.length() > 0 && name.charAt(0) == '_' && name.length() > 1 && name.charAt(1) != '_';
		boolean privateSymbol = seemsToBePrivateSymbol;

		if(seemsToBePrivateSymbol)
		{
			if(!(myTargetFile instanceof JSFile))
			{
				// no interest in html, jsp for private symbols
				return;
			}

			if(myContextNameIds == null || myContextNameIds.length == 0 || myContextNameIds[myContextNameIds.length - 1].equals(namespace.getNameId()))
			{
				privateSymbol = false;
			}
		}

		JSNamedElementProxy.NamedItemType type = null;
		if(element instanceof JSNamedElementProxy)
		{
			type = ((JSNamedElementProxy) element).getType();
		}

		if(myProcessOnlyProperties)
		{
			if(type != null)
			{
				if(type != JSNamedElementProxy.NamedItemType.Property && type != JSNamedElementProxy.NamedItemType.Definition)
				{
					return; // no interest when complete existing property name
				}
			}
		}

		if(myProcessOnlyTypes && ecmal4)
		{
			if(type != null)
			{

				boolean nonAcceptableItem = (type != JSNamedElementProxy.NamedItemType.Clazz && type != JSNamedElementProxy.NamedItemType.Namespace) || "Arguments".equals(((JSNamedElement) element)
						.getName());
				if(nonAcceptableItem && !myStrictTypeContext)
				{
					nonAcceptableItem = type != JSNamedElementProxy.NamedItemType.Function && type != JSNamedElementProxy.NamedItemType.Variable;
				}

				if(!nonAcceptableItem && type == JSNamedElementProxy.NamedItemType.Clazz)
				{
					final boolean isInterface = ((JSNamedElementProxy) element).hasProperty(JSNamedElementProxy.Property.Interface);

					if((isInterface && myProcessOnlyClasses) || (!isInterface && myProcessOnlyInterfaces))
					{
						nonAcceptableItem = true;
					}
				}

				if(nonAcceptableItem)
				{
					return;
				}
			}
		}

		MatchType matchType = isAcceptableQualifiedItem(namespace, type);

		if(matchType == MatchType.COMPLETE && !ecmal4 && myProcessOnlyTypes)
		{
			if(((name != null && name.length() > 0 && Character.isLowerCase(name.charAt(0))) || seemsToBePrivateSymbol) && (type == null || type != JSNamedElementProxy.NamedItemType.Namespace))
			{
				matchType = MatchType.PARTIAL;
			}
		}

		if(matchType == MatchType.PARTIAL || (privateSymbol && matchType == MatchType.COMPLETE))
		{
			addPartialMatch(element, nameId);

			if(!ecmal4 &&
					namespace.getNameId() != null &&
					!myNames2CandidatesMap.containsKey(namespace.getNameId()) &&
					isAcceptableNS(namespace) == MatchType.COMPLETE_NS &&
					namespace.getNameId().length() > 0)
			{
				addCompleteMatch(namespace, namespace.getNameId());
			}
		}
		else if(matchType == MatchType.COMPLETE)
		{
			addCompleteMatch(element, nameId);
		}
	}


	private Object addLookupValue(PsiElement _element, final String nameId, JSLookupUtil.LookupPriority priority)
	{
		PsiElement element = _element;
		boolean proxyExpanded = false;

		if(element instanceof JSNamedElementProxy && element.getContainingFile() == myTargetFile)
		{ // expand proxies from completion target file, since they will be invalidated by typing
			element = ((JSNamedElementProxy) element).getElement();
			proxyExpanded = true;
			if(element == null)
			{
				return null; // something weird happened
			}
		}

		final Object item = JSLookupUtil.createLookupItem(element, nameId, priority);

		if(item != null && proxyExpanded)
		{
			element.putUserData(JSElementImpl.ORIGINAL_ELEMENT, (NavigationItem) _element);
		}

		return item;
	}


	@Override
	protected String[] calculateContextIds(final JSReferenceExpression jsReferenceExpression)
	{
		return JSResolveUtil.buildNameIdsForQualifier(JSResolveUtil.getRealRefExprQualifier(jsReferenceExpression), myIndex);
	}

	@Override
	protected boolean isFromRelevantFileOrDirectory()
	{
		return super.isFromRelevantFileOrDirectory(); // || myTargetFiles.contains(myCurrentFile);
	}

	private void addCompleteMatch(final Object _element, String nameId)
	{
		addCompleteMatch(_element, nameId, true);
	}

	private void addCompleteMatch(final Object _element, String nameId, boolean doFilterting)
	{
		if(!doAdd(_element, nameId, doFilterting))
		{
			boolean removedFromPartialNames = false;
			final Object el = myNames2CandidatesMap.get(nameId);

			if(el != null)
			{
				removedFromPartialNames = myPartialMatchNamesMapFromSameFile.remove(nameId) != null || myPartialMatchNamesMap.remove(nameId) != null;
			}
			if(!removedFromPartialNames)
			{
				return;
			}
		}

		PsiElement element = updateElement(_element);

		if(isFromRelevantFileOrDirectory() && !myAddOnlyCompleteMatches)
		{
			final Object o = addLookupValue(element, nameId, JSLookupUtil.LookupPriority.HIGHEST);
			if(o != null)
			{
				myNamesList.add(myThisFileNameListCount++, o);
			}
			else
			{
				myNames2CandidatesMap.remove(nameId);
			}
		}
		else
		{
			final Object o = addLookupValue(element, nameId, JSLookupUtil.LookupPriority.HIGH);
			if(o != null)
			{
				myNamesList.add(o);
			}
			else
			{
				myNames2CandidatesMap.remove(nameId);
			}
		}
	}

	private PsiElement updateElement(final Object _element)
	{
		PsiElement element;
		if(_element instanceof JSNamedElementProxy)
		{
			final JSNamedElementProxy proxy = (JSNamedElementProxy) _element;

			if(proxy.getType() == JSNamedElementProxy.NamedItemType.ImplicitVariable || proxy.getType() == JSNamedElementProxy.NamedItemType.ImplicitFunction)
			{
				element = new MyElementWrapper(proxy);
			}
			else
			{
				element = proxy;
			}
		}
		else if(_element instanceof PsiElement)
		{
			element = (PsiElement) _element;
		}
		else if(_element instanceof String)
		{
			element = new MyElementWrapper((String) _element, myCurrentFile, 0);
		}
		else
		{
			element = new MyElementWrapper((JSNamespace) _element, myCurrentFile, 0);
		}
		return element;
	}

	private boolean doAdd(Object element, String nameId, boolean doFilterting)
	{
		if(nameId == null || (doFilterting && myNames2CandidatesMap.get(nameId) != null))
		{
			return false;
		}
		myNames2CandidatesMap.put(nameId, element);
		return true;
	}

	private void addPartialMatch(final PsiElement _element, String nameId)
	{
		if(myAddOnlyCompleteMatches)
		{
			return;
		}
		if(!doAdd(_element, nameId, true))
		{
			return;
		}

		PsiElement element = updateElement(_element);

		final Map<String, Object> targetNamesMap;
		final JSLookupUtil.LookupPriority priority;

		if(isFromRelevantFileOrDirectory())
		{
			priority = hasSomeSmartnessAvailable ? JSLookupUtil.LookupPriority.HIGHER : JSLookupUtil.LookupPriority.HIGHEST;
			targetNamesMap = myPartialMatchNamesMapFromSameFile;
		}
		else
		{
			priority = hasSomeSmartnessAvailable ? JSLookupUtil.LookupPriority.NORMAL : JSLookupUtil.LookupPriority.HIGH;

			targetNamesMap = myPartialMatchNamesMap;
		}

		final Object o = addLookupValue(element, nameId, priority);

		if(o != null)
		{
			targetNamesMap.put(nameId, o);
		}
		else
		{
			myNames2CandidatesMap.remove(nameId);
		}
	}

	private MatchType isAcceptableQualifiedItem(JSNamespace namespace, final JSNamedElementProxy.NamedItemType type)
	{
		if(myProcessOnlyTypes && ecmal4)
		{
			return MatchType.COMPLETE;
		}

		if(!hasSomeInfoAvailable || myDefinitelyGlobalReference)
		{
			return isGlobalNS(namespace) || (ecmal4 && (type == JSNamedElementProxy.NamedItemType.Variable ||
					type == JSNamedElementProxy.NamedItemType.Function ||
					type == JSNamedElementProxy.NamedItemType.Clazz)) ? MatchType.COMPLETE : (myDefinitelyGlobalReference ? MatchType.NOMATCH : MatchType.PARTIAL);
		}

		if(namespace.getParent() == null)
		{
			return MatchType.NOMATCH;
		}

		for(String[] myNameComponents : myNameIdsArray)
		{
			boolean completeMatch = true;
			JSNamespace currentNs = namespace;
			int i;

			for(i = myNameComponents.length - 1; i >= 0; --i)
			{
				if(myNameComponents[i].equals(currentNs.getNameId()))
				{
					currentNs = currentNs.getParent();
					if(currentNs != null)
					{
						continue;
					}
				}
				completeMatch = false;
				break;
			}

			if(completeMatch && currentNs != namespace)
			{
				if(myAddOnlyCompleteMatches && (i >= 0 || currentNs.getNameId() != null))
				{
					return MatchType.PARTIAL;
				}
				return MatchType.COMPLETE;
			}
		}

		return MatchType.PARTIAL;
	}

	private MatchType isAcceptableNS(JSNamespace namespace)
	{
		boolean completeMatch = false;
		JSNamespace currentNs = namespace;

		while(true)
		{
			final JSNamespace newParentCandidate = currentNs.getParent();
			if(newParentCandidate == null)
			{
				break;
			}
			if(newParentCandidate.getNameId() == null)
			{
				break;
			}
			currentNs = newParentCandidate;
		}

		if(myContextNameIds == null || myContextNameIds.length == 0)
		{
			return MatchType.COMPLETE_NS;
		}
		if(currentNs == namespace)
		{
			return MatchType.NOMATCH;
		}

		for(int i = 0; i < myContextNameIds.length; ++i)
		{
			if(myContextNameIds[i].equals(currentNs.getNameId()))
			{
				if(i + 1 < myContextNameIds.length)
				{
					currentNs = currentNs.findChildNamespace(myContextNameIds[i + 1]);
					if(currentNs == null)
					{
						break;
					}
					if(currentNs == namespace)
					{
						break;
					}
				}
				else
				{
					completeMatch = namespace.getParent() == currentNs;
					break;
				}
			}
			else
			{
				break;
			}
		}

		if(completeMatch)
		{
			return MatchType.COMPLETE_NS;
		}

		return MatchType.NOMATCH;
	}

	public static class MyElementWrapper extends PsiElementBase implements JSNamedElement
	{
		private final JSNamespace myJsNamespace;
		private final String myArtificialName;
		private final JSNamedElementProxy myProxy;
		private int myOffset;
		private PsiFile myCurrentFile;

		@Override
		public boolean equals(final Object obj)
		{
			if(!(obj instanceof MyElementWrapper))
			{
				return false;
			}
			final MyElementWrapper elementWrapper = (MyElementWrapper) obj;

			return elementWrapper.myOffset == myOffset;
		}

		@Override
		public int hashCode()
		{
			return myOffset;
		}

		public MyElementWrapper(final JSNamespace jsNamespace, PsiFile file, int offset)
		{
			myJsNamespace = jsNamespace;
			myProxy = null;
			myOffset = offset;
			myCurrentFile = file;
			myArtificialName = null;
		}

		public MyElementWrapper(final String name, PsiFile file, int offset)
		{
			myJsNamespace = null;
			myProxy = null;
			myOffset = offset;
			myCurrentFile = file;
			myArtificialName = name;
		}

		public MyElementWrapper(JSNamedElementProxy _proxy)
		{
			myProxy = _proxy;
			myJsNamespace = null;
			myArtificialName = null;
		}

		@RequiredReadAction
		@Override
		@NotNull
		public Language getLanguage()
		{
			return JavaScriptLanguage.INSTANCE;
		}

		@RequiredReadAction
		@Override
		@NotNull
		public PsiElement[] getChildren()
		{
			return PsiElement.EMPTY_ARRAY;
		}

		@Override
		public PsiElement getParent()
		{
			return myCurrentFile != null ? myCurrentFile : myProxy.getContainingFile();
		}

		@RequiredReadAction
		@Override
		@Nullable
		public PsiElement getFirstChild()
		{
			return null;
		}

		@RequiredReadAction
		@Override
		@Nullable
		public PsiElement getLastChild()
		{
			return null;
		}

		@Override
		@NotNull
		public Project getProject()
		{
			return getParent().getProject();
		}

		@RequiredReadAction
		@Override
		@Nullable
		public PsiElement getNextSibling()
		{
			return null;
		}

		@RequiredReadAction
		@Override
		@Nullable
		public PsiElement getPrevSibling()
		{
			return null;
		}

		@RequiredReadAction
		@Override
		public TextRange getTextRange()
		{
			return new TextRange(myOffset, myOffset);
		}

		@RequiredReadAction
		@Override
		public int getStartOffsetInParent()
		{
			return myOffset;
		}

		@RequiredReadAction
		@Override
		public int getTextLength()
		{
			return 0;
		}

		@RequiredReadAction
		@Override
		@Nullable
		public PsiElement findElementAt(final int offset)
		{
			return null;
		}

		@Override
		public int getTextOffset()
		{
			return myOffset;
		}

		@RequiredReadAction
		@Override
		@NonNls
		public String getText()
		{
			if(myArtificialName != null)
			{
				return myArtificialName;
			}
			return myJsNamespace != null ? myJsNamespace.getNameId() : myProxy.getNameId();
		}

		@RequiredReadAction
		@Override
		@NotNull
		public char[] textToCharArray()
		{
			return new char[0];
		}

		@RequiredReadAction
		@Override
		public boolean textContains(final char c)
		{
			return false;
		}

		@Override
		@Nullable
		public ASTNode getNode()
		{
			return null;
		}

		@Override
		public String getName()
		{
			return getText();
		}

		@Override
		public boolean isPhysical()
		{
			return true;
		}

		@Override
		public PsiElement setName(@NonNls @NotNull String name) throws IncorrectOperationException
		{
			throw new UnsupportedOperationException();
		}

		@Override
		public PsiElement getNameIdentifier()
		{
			return null;
		}

		@Override
		public ItemPresentation getPresentation()
		{
			if(myArtificialName != null)
			{
				return null;
			}
			return myJsNamespace != null ? new ItemPresentation()
			{
				final JavaScriptIndex myIndex = JavaScriptIndex.getInstance(getProject());

				@Override
				public String getPresentableText()
				{
					return myJsNamespace.getQualifiedName(myIndex);
				}

				@Override
				@Nullable
				public String getLocationString()
				{
					return myJsNamespace.getParent().getQualifiedName(myIndex);
				}

				@Override
				@Nullable
				@RequiredReadAction
				public Icon getIcon(final boolean open)
				{
					return AllIcons.Nodes.Class;
				}
			} : myProxy.getPresentation();
		}

		public JSNamedElementProxy getProxy()
		{
			return myProxy;
		}

		public String getArtificialName()
		{
			return myArtificialName;
		}

		public JSNamespace getNamespace()
		{
			return myJsNamespace;
		}

		@Override
		@Nullable
		public ASTNode findNameIdentifier()
		{
			return null;
		}
	}
}
