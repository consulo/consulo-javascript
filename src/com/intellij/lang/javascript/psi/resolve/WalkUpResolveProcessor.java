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

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.ResolveState;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.util.ArrayUtil;

/**
 * @author Maxim.Mossienko
 */
public class WalkUpResolveProcessor extends BaseJSSymbolProcessor
{
	protected String myReferenceName;
	protected String[][] myContextIds;
	private int myFilePartialResultsCount;
	private List<ResolveResult> myPartialMatchResults;
	private int myFileCompleteResultsCount;
	private List<ResolveResult> myCompleteMatchResults;

	private int myBestMatchedContextId = -1;
	private boolean myCanHaveValidPartialMatches;
	private boolean embeddedToHtmlAttr;
	private boolean myInNewExpression;

	public WalkUpResolveProcessor(String referenceName, String[] contextIds, PsiFile targetFile, boolean skipDclsInTargetFile, PsiElement context)
	{
		super(targetFile, skipDclsInTargetFile, context, contextIds);
		contextIds = myContextNameIds;

		myReferenceName = referenceName;

		if(context instanceof JSReferenceExpression)
		{
			myInNewExpression = context.getParent() instanceof JSNewExpression;

			final List<String[]> possibleNameIds = new ArrayList<String[]>(1);
			final JSReferenceExpression refExpr = (JSReferenceExpression) context;
			final JSExpression originalQualifier = refExpr.getQualifier();
			final JSExpression qualifier = JSResolveUtil.getRealRefExprQualifier(refExpr);

			final JSClass jsClass = PsiTreeUtil.getParentOfType(context, JSClass.class); // get rid of it!
			final JSElement file = PsiTreeUtil.getParentOfType(context, JSEmbeddedContentImpl.class, JSFile.class);

			if((file instanceof JSFile && file.getContext() instanceof XmlAttributeValue) || file instanceof JSEmbeddedContentImpl)
			{
				embeddedToHtmlAttr = true;
			}

			boolean haveNotEncounteredDynamics = true;
			if(qualifier instanceof JSThisExpression ||
					qualifier instanceof JSSuperExpression ||
					qualifier == null ||
					originalQualifier == null)
			{
				myDefinitelyNonglobalReference = false;

				if(qualifier instanceof JSThisExpression)
				{
					JSResolveUtil.ContextResolver resolver = new JSResolveUtil.ContextResolver(qualifier);
					final String contextQualifierText = resolver.getQualifierAsString();
					final PsiElement clazz = contextQualifierText != null ? JSClassImpl.findClassFromNamespace(contextQualifierText, context) : null;

					if(clazz instanceof JSClass)
					{
						JSAttributeList attrList;

						if((attrList = ((JSClass) clazz).getAttributeList()) != null)
						{
							final boolean clazzIsDynamic = attrList.hasModifier(JSAttributeList.ModifierType.DYNAMIC);
							if(clazzIsDynamic)
							{
								haveNotEncounteredDynamics = false;
							}
						}

						buildIndexListFromQNameAndCorrectQName(contextQualifierText, clazz, possibleNameIds);
					}
					else
					{
						possibleNameIds.add(contextIds);
					}
				}

				if(qualifier == null)
				{
					final JSImportedElementResolveResult expression = JSImportHandlingUtil.resolveTypeNameUsingImports(refExpr);

					if(expression != null)
					{
						possibleNameIds.add(JSResolveUtil.buildNameIdsForQualifier(JSResolveUtil.getRealRefExprQualifierFromResult(refExpr, expression), myIndex));
					}
					else
					{
						addPackageScope(possibleNameIds, jsClass, refExpr);
					}
				}
				else if(originalQualifier == null)
				{
					if(refExpr.getNode().getFirstChildNode().getElementType() == JSTokenTypes.AT)
					{
						haveNotEncounteredDynamics = false;
					}
					possibleNameIds.add(contextIds);
				}

				if(contextIds != null)
				{
					iterateContextIds(contextIds, possibleNameIds, true);
				}
				if(originalQualifier == null)
				{
					possibleNameIds.add(ArrayUtil.EMPTY_STRING_ARRAY);
				}
			}
			else if(!ecmal4)
			{
				VariantsProcessor.doEvalForExpr(qualifier, myTargetFile, new VariantsProcessor.TypeProcessor()
				{
					@Override
					public void process(@NotNull String type, @NotNull final EvaluateContext context, final PsiElement source)
					{
						if(context.visitedTypes.contains(type))
						{
							return;
						}
						context.visitedTypes.add(type);

						if("window".equals(type))
						{
							possibleNameIds.add(new String[0]);
							return;
						}
						type = buildIndexListFromQNameAndCorrectQName(type, source, possibleNameIds);

						doIterateHierarchy(type, new HierarchyProcessor()
						{
							@Override
							public boolean processNamespace(final JSNamespace ns)
							{
								possibleNameIds.add(ns.getIndices());
								return true;
							}

							@Override
							public boolean processClass(final JSClass clazz)
							{
								buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameIds);
								return true;
							}
						});
					}

					@Override
					public void setUnknownElement(@NotNull final PsiElement element)
					{
					}

					@Override
					public boolean ecma()
					{
						return ecmal4;
					}
				});
			}
			if(possibleNameIds.size() != 0)
			{
				myContextIds = possibleNameIds.toArray(new String[possibleNameIds.size()][]);
				myAddOnlyCompleteMatches = haveNotEncounteredDynamics;
				myCanHaveValidPartialMatches = !ecmal4 && !myDefinitelyGlobalReference || !haveNotEncounteredDynamics;
			}
			else if(myContextIds == null && contextIds != null)
			{
				myContextIds = new String[][]{contextIds};
			}
		}
		else if(contextIds != null)
		{
			final List<String[]> possibleNameIds = new ArrayList<String[]>(1);
			possibleNameIds.add(contextIds);
			iterateContextIds(contextIds, possibleNameIds, false);
			myContextIds = possibleNameIds.toArray(new String[possibleNameIds.size()][]);
		}
	}

	private void iterateContextIds(final String[] contextIds, final List<String[]> possibleNameIds, final boolean allowObject)
	{
		doIterateTypeHierarchy(contextIds, new HierarchyProcessor()
		{
			@Override
			public boolean processNamespace(final JSNamespace ns)
			{
				if("Object".equals(ns.getQualifiedName(myIndex)) && !allowObject)
				{
					return true;
				}
				possibleNameIds.add(ns.getIndices());
				return true;
			}

			@Override
			public boolean processClass(final JSClass clazz)
			{
				buildIndexListFromQNameAndCorrectQName(clazz.getQualifiedName(), clazz, possibleNameIds);
				return true;
			}
		});
	}

	@Override
	public boolean processFunction(JSNamespace namespace, final String nameId, final JSNamedElement function)
	{
		doQualifiedCheck(namespace, nameId, function);
		return true;
	}

	@Override
	public boolean processClass(final JSNamespace namespace, final String nameId, final JSNamedElement clazz)
	{
		doQualifiedCheck(namespace, nameId, clazz);
		return true;
	}

	protected MatchType isAcceptableQualifiedItem(JSNamespace namespace, final String nameId, final PsiElement element)
	{
		final boolean partialMatch = myReferenceName.equals(nameId);

		if(partialMatch)
		{
			if(myInNewExpression)
			{
				final JSNamedElementProxy proxy = (JSNamedElementProxy) element;
				final JSNamedElementProxy.NamedItemType type = proxy.getType();

				final boolean qualifiedContext = ((JSReferenceExpression) myContext).getQualifier() != null;

				if(type == JSNamedElementProxy.NamedItemType.MemberFunction && proxy.hasProperty(JSNamedElementProxy.Property.Constructor))
				{
					final boolean inglobalNs = isGlobalNS(namespace.getParent());
					final boolean canResolveToElement = !qualifiedContext == inglobalNs;
					return canResolveToElement ? MatchType.COMPLETE : MatchType.NOMATCH;
				}
				else if(type == JSNamedElementProxy.NamedItemType.Clazz)
				{
					final boolean inglobalNs = isGlobalNS(namespace);
					final boolean canResolveToElement = !qualifiedContext == inglobalNs;
					return proxy.hasProperty(JSNamedElementProxy.Property.HasConstructor) || !canResolveToElement ? MatchType.NOMATCH : MatchType.COMPLETE;
				}
			}

			int i = -1;

			if(myContextIds != null)
			{
				int maxContextScanCount = myBestMatchedContextId == -1 ? myContextIds.length : myBestMatchedContextId + 1;

				for(int currentContextIndex = 0; currentContextIndex < maxContextScanCount; ++currentContextIndex)
				{
					final String[] contextIds = myContextIds[currentContextIndex];

					for(i = contextIds.length - 1; i >= 0; --i)
					{
						if(namespace == null)
						{
							break;
						}
						if(!namespace.getNameId().equals(contextIds[i]))
						{
							break;
						}
						namespace = namespace.getParent();
					}

					if(i < 0 && isGlobalNS(namespace))
					{
						if(myBestMatchedContextId == -1)
						{
							myBestMatchedContextId = currentContextIndex;
						}
						else if(currentContextIndex < myBestMatchedContextId)
						{
							myBestMatchedContextId = currentContextIndex;
							myCompleteMatchResults = null;
							myFileCompleteResultsCount = 0;
						}
						return MatchType.COMPLETE;
					}
				}
			}

			if(i < 0 && isGlobalNS(namespace))
			{
				return MatchType.COMPLETE;
			}
		}

		return partialMatch ? MatchType.PARTIAL : MatchType.NOMATCH;
	}

	private void doQualifiedCheck(final JSNamespace namespace, String nameId, final PsiElement element)
	{
		final MatchType matchType = isAcceptableQualifiedItem(namespace, nameId, element);

		if(matchType == MatchType.PARTIAL)
		{
			JSNamedElementProxy.NamedItemType type;

			if(myDefinitelyGlobalReference && ((element instanceof JSNamedElementProxy && ((type = ((JSNamedElementProxy) element).getType()) == JSNamedElementProxy.NamedItemType.FunctionProperty ||
					type == JSNamedElementProxy.NamedItemType.Property ||
					type == JSNamedElementProxy.NamedItemType.MemberFunction ||
					type == JSNamedElementProxy.NamedItemType.MemberVariable ||
					(type == JSNamedElementProxy.NamedItemType.Definition && !isGlobalNS(namespace)))) || (element instanceof JSFunctionExpression ||
					element instanceof JSProperty ||
					(element instanceof JSDefinitionExpression && !isGlobalNS(namespace)))))
			{
				return; // nonqualified item could not be resolved into property
			}

			if(myDefinitelyNonglobalReference && (element instanceof JSNamedElementProxy && ((type = ((JSNamedElementProxy) element).getType()) == JSNamedElementProxy.NamedItemType.Variable || type
					== JSNamedElementProxy.NamedItemType.Function)) || (element instanceof JSVariable || (element instanceof JSFunction && !(element instanceof JSFunctionExpression))))
			{
				return; // qualified item could not be resolved into function/variable
			}
			addPartialResult(element);
		}
		else if(matchType == MatchType.COMPLETE)
		{
			addCompleteResult(element);
		}
	}

	private void addCompleteResult(PsiElement element)
	{
		final JSResolveUtil.MyResolveResult o = new JSResolveUtil.MyResolveResult(element);
		addCompleteResult(o);
	}

	private void addCompleteResult(ResolveResult o)
	{
		if(myCompleteMatchResults == null)
		{
			myCompleteMatchResults = new ArrayList<ResolveResult>(1);
		}
		if(isFromRelevantFileOrDirectory())
		{
			myCompleteMatchResults.add(myFileCompleteResultsCount++, o);
		}
		else
		{
			myCompleteMatchResults.add(o);
		}
	}

	private void addPartialResult(PsiElement element)
	{
		if(myPartialMatchResults == null)
		{
			myPartialMatchResults = new ArrayList<ResolveResult>(1);
		}
		final JSResolveUtil.MyResolveResult o = new JSResolveUtil.MyResolveResult(element, !myAddOnlyCompleteMatches);

		if(isFromRelevantFileOrDirectory())
		{
			myPartialMatchResults.add(myFilePartialResultsCount++, o);
		}
		else
		{
			myPartialMatchResults.add(o);
		}
	}

	@Override
	public boolean processProperty(JSNamespace namespace, final String nameId, JSNamedElement property)
	{
		doQualifiedCheck(namespace, nameId, property);
		return true;
	}

	@Override
	public boolean processVariable(JSNamespace namespace, final String nameId, JSNamedElement variable)
	{
		if(namespace.getNameId() != null)
		{
			doQualifiedCheck(namespace, nameId, variable);
		}
		else if(shouldProcessVariable(nameId, variable))
		{
			addCompleteResult(variable);
		}
		return true;
	}

	@Override
	public PsiFile getBaseFile()
	{
		return myTargetFile;
	}

	@Override
	public boolean processDefinition(final JSNamespace namespace, final String nameId, final JSNamedElement refExpr)
	{
		//if(myCurrentFile != myTargetFile || !mySkipDclsInTargetFile)
		doQualifiedCheck(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processNamespace(final JSNamespace namespace, final String nameId, final JSNamedElement refExpr)
	{
		doQualifiedCheck(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitNamespace(final JSNamespace namespace, final String nameId, final PsiElement refExpr, boolean finalReference)
	{
		doQualifiedCheck(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitFunction(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
	{
		doQualifiedCheck(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public boolean processImplicitVariable(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
	{
		doQualifiedCheck(namespace, nameId, refExpr);
		return true;
	}

	@Override
	public String getRequiredNameId()
	{
		return myReferenceName;
	}

	@Override
	public boolean processTag(JSNamespace namespace, final String nameId, PsiNamedElement namedElement, final String attrName)
	{
		doQualifiedCheck(namespace, nameId, namedElement);
		return true;
	}

	protected boolean shouldProcessVariable(final String nameId, JSNamedElement var)
	{
		return (myReferenceName.equals(nameId) && (!myDefinitelyNonglobalReference || (var instanceof JSNamedElementProxy && ((JSNamedElementProxy) var).getType() == JSNamedElementProxy
				.NamedItemType.MemberVariable)));
	}

	public ResolveResult[] getResults()
	{
		int resultCount = 0;
		if(myCompleteMatchResults != null)
		{
			resultCount += myCompleteMatchResults.size();
		}
		final boolean addPartialResults = !myAddOnlyCompleteMatches || (resultCount == 0 && myAllowPartialResults);
		if(myPartialMatchResults != null && addPartialResults)
		{
			resultCount += myPartialMatchResults.size();
		}

		final ResolveResult[] result = resultCount != 0 ? new ResolveResult[resultCount] : ResolveResult.EMPTY_ARRAY;

		if(myCompleteMatchResults != null)
		{
			for(int i = 0; i < myCompleteMatchResults.size(); ++i)
			{
				result[i] = myCompleteMatchResults.get(i);
				assert result[i] != null;
			}
		}

		if(myPartialMatchResults != null && addPartialResults)
		{
			int offset = myCompleteMatchResults != null ? myCompleteMatchResults.size() : 0;
			for(int i = 0; i < myPartialMatchResults.size(); ++i)
			{
				final JSResolveUtil.MyResolveResult resolveResult = (JSResolveUtil.MyResolveResult) myPartialMatchResults.get(i);
				JSNamedElementProxy.NamedItemType type;

				if(myCompleteMatchResults == null && (myCanHaveValidPartialMatches || (resolveResult.getElement() instanceof JSNamedElementProxy && (//(type = (
						// (JSNamedElementProxy)resolveResult.getElement()).getType()) == JSNamedElementProxy.NamedItemType.Namespace ||
						(((JSNamedElementProxy) resolveResult.getElement()).getType() == JSNamedElementProxy.NamedItemType.FunctionExpression && embeddedToHtmlAttr)))))
				{
					resolveResult.setValid(true);
				}
				assert resolveResult != null;
				result[offset + i] = resolveResult;
			}
		}

		return result;
	}

	@Override
	protected String[] calculateContextIds(final JSReferenceExpression jsReferenceExpression)
	{
		String[] contextNameIds = null;
		JSExpression qualifier = JSResolveUtil.getRealRefExprQualifier(jsReferenceExpression);

		if(qualifier instanceof JSReferenceExpression)
		{
			contextNameIds = JSSymbolUtil.buildNameIndexArray(qualifier, null, myIndex);
		}
		else if(qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression)
		{
			contextNameIds = JSResolveUtil.buildNameIdsForQualifier(qualifier, myIndex);
		}

		return contextNameIds;
	}

	public void addLocalResults(final ResolveResult results[])
	{
		if(results == null)
		{
			return;
		}
		for(ResolveResult e : results)
		{
			addCompleteResult(e);
		}
	}

	public int getCompleteResultCount()
	{
		return myCompleteMatchResults == null ? 0 : myCompleteMatchResults.size();
	}

	@Override
	public boolean execute(final PsiElement element, final ResolveState state)
	{
		if((element instanceof JSNamedElement && myReferenceName.equals(((JSNamedElement) element).getName())) || element == myContext)
		{
			addCompleteResult(element);
		}
		return true;
	}
}
