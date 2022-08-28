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

package com.intellij.lang.javascript.impl.highlighting;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.search.JSClassSearch;
import com.intellij.lang.javascript.impl.search.JSFunctionsSearch;
import consulo.application.AllIcons;
import consulo.application.ApplicationManager;
import consulo.application.progress.ProgressManager;
import consulo.application.util.function.Processor;
import consulo.application.util.query.CollectionQuery;
import consulo.application.util.query.Query;
import consulo.language.editor.Pass;
import consulo.language.editor.gutter.GutterIconNavigationHandler;
import consulo.language.editor.gutter.LineMarkerInfo;
import consulo.language.editor.gutter.LineMarkerProvider;
import consulo.language.editor.ui.DefaultPsiElementCellRenderer;
import consulo.language.editor.ui.PopupNavigationUtil;
import consulo.language.editor.ui.PsiElementListNavigator;
import consulo.language.psi.NavigatablePsiElement;
import consulo.language.psi.PsiElement;
import consulo.navigation.NavigationItem;
import consulo.ui.ex.RelativePoint;
import consulo.util.dataholder.Key;
import consulo.util.lang.ref.Ref;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.function.Function;

/**
 * @author Maxim.Mossienko
 * Date: Apr 14, 2008
 * Time: 11:43:47 PM
 */
public abstract class JavaScriptLineMarkerProvider implements LineMarkerProvider
{
	@NonNls
	private static final String OVERRIDES_METHOD_IN = "overrides method in ";

	private static final Function<JSClass, String> ourClassInheritorsTooltipProvider = clazz -> "Has subclasses";

	private static final Function<JSClass, String> ourImplementedInterfacesTooltipProvider = clazz -> "Has implementations";

	private static final Function<JSFunction, String> ourOverriddenFunctionsTooltipProvider = psiElement -> "Is overridden";

	private static final Function<JSFunction, String> ourImplementingFunctionsTooltipProvider = psiElement -> "Is implemented";

	private static final BasicGutterIconNavigationHandler<JSClass> ourClassInheritorsNavHandler = new BasicGutterIconNavigationHandler<JSClass>()
	{
		@Override
		protected String getTitle(final JSClass elt)
		{
			return "Choose Subclass of " + elt.getName();
		}

		@Override
		protected Query<JSClass> search(final JSClass elt)
		{
			return JSClassSearch.searchClassInheritors(elt, true);
		}
	};

	private static final BasicGutterIconNavigationHandler<JSClass> ourInterfaceImplementationsNavHandler = new
			BasicGutterIconNavigationHandler<JSClass>()
			{
				@Override
				protected String getTitle(final JSClass elt)
				{
					return "Choose Implementation of " + elt.getName();
				}

				@Override
				protected Query<JSClass> search(final JSClass elt)
				{
					return JSClassSearch.searchInterfaceImplementations(elt, true);
				}
			};

	private static final BasicGutterIconNavigationHandler<JSFunction> ourOverriddenFunctionsNavHandler = new
			BasicGutterIconNavigationHandler<JSFunction>()
			{
				@Override
				protected String getTitle(final JSFunction elt)
				{
					return "Choose Overriden Function of " + elt.getName();
				}

				@Override
				protected Query<JSFunction> search(final JSFunction elt)
				{
					return doFindOverridenFunctionStatic(elt);
				}
			};
	private final boolean myUnitTestMode = ApplicationManager.getApplication().isUnitTestMode();


	public static Query<JSFunction> doFindOverridenFunctionStatic(final JSFunction elt)
	{
		PsiElement parent = JSResolveUtil.findParent(elt);
		if(parent instanceof JSClass)
		{
			return JSFunctionsSearch.searchOverridingFunctions(elt, true);
		}
		final String qName = JSResolveUtil.getQNameToStartHierarchySearch(elt);
		if(qName != null)
		{
			final ArrayList<JSFunction> result = new ArrayList<>();

			return new CollectionQuery<>(result);
		}

		return new CollectionQuery<>(Collections.<JSFunction>emptyList());
	}

	private static final BasicGutterIconNavigationHandler<JSFunction> ourImplementingFunctionsNavHandler = new
			BasicGutterIconNavigationHandler<JSFunction>()
			{
				@Override
				protected String getTitle(final JSFunction elt)
				{
					return "Choose Implementation of " + elt.getName();
				}

				@Override
				protected Query<JSFunction> search(final JSFunction elt)
				{
					return JSFunctionsSearch.searchImplementingFunctions(elt, true);
				}
			};

	public static Key<Boolean> ourParticipatesInHierarchyKey = Key.create("js.named.item.participates.in.hierarchy");

	@Override
	public LineMarkerInfo getLineMarkerInfo(@Nonnull final PsiElement element)
	{
		if(element instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) element;
			function.putUserData(ourParticipatesInHierarchyKey, null);
			if(function.getNameIdentifier() == null)
			{
				return null;
			}
			final String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);

			if(qName != null)
			{
				PsiElement parentNode = element.getParent();
				if(parentNode instanceof JSFile)
				{
					JSClass xmlBackedClass = JSResolveUtil.getXmlBackedClass((JSFile) parentNode);
					if(xmlBackedClass != null)
					{
						parentNode = xmlBackedClass;
					}
				}

				if(element instanceof JSFunctionExpression)
				{
					parentNode = element.getContainingFile();
				}

				final MyOverrideHandler overrideHandler = new MyOverrideHandler();
				final String typeName = parentNode instanceof JSClass ? ((JSClass) parentNode).getQualifiedName() : qName;
				JSResolveUtil.iterateType(function, parentNode, typeName, overrideHandler);

				if(overrideHandler.className != null)
				{
					final PsiElement parentNode1 = parentNode;
					function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);

					return new LineMarkerInfo<>(function, function.getNameIdentifier().getTextRange().getStartOffset(), AllIcons.Gutter.OverridingMethod,
							Pass.UPDATE_ALL, psiElement -> OVERRIDES_METHOD_IN + overrideHandler.className, new GutterIconNavigationHandler<JSFunction>()
					{
						@Override
						public void navigate(final MouseEvent e, final JSFunction elt)
						{
							final Set<NavigationItem> results = new HashSet<>();
							JSResolveUtil.iterateType(function, parentNode1, typeName, new JSResolveUtil.OverrideHandler()
							{
								@Override
								public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
								{
									for(PsiElement e : processor.getResults())
									{
										results.add((NavigationItem) e);
									}
									return true;
								}
							});

							if(results.size() == 1)
							{
								results.iterator().next().navigate(true);
							}
							else if(results.size() > 1)
							{
								PopupNavigationUtil.getPsiElementPopup(results.toArray(new PsiElement[results.size()]), "Choose super class or interface").show(new RelativePoint
										(e));
							}
						}
					}
					);
				}
			}
		}

		return null;
	}

	@Override
	public void collectSlowLineMarkers(@Nonnull final List<PsiElement> elements, @Nonnull final Collection<LineMarkerInfo> result)
	{
		final Map<String, Set<JSFunction>> jsFunctionsToProcess = new HashMap<>();
		final Map<JSClass, Set<JSFunction>> jsMethodsToProcess = new HashMap<>();

		for(final PsiElement el : elements)
		{
			ProgressManager.getInstance().checkCanceled();

			if(el instanceof JSFunction)
			{
				final JSFunction function = (JSFunction) el;
				if(isNotApplicableForOverride(function))
				{
					continue;
				}

				PsiElement parent = function.getParent();
				if(parent instanceof JSFile)
				{
					parent = JSResolveUtil.getClassReferenceForXmlFromContext(parent);
				}

				if(parent instanceof JSClass)
				{
					final JSClass clazz = (JSClass) parent;

					Set<JSFunction> functions = jsMethodsToProcess.get(clazz);
					if(functions == null)
					{
						functions = new HashSet<>();
						jsMethodsToProcess.put(clazz, functions);
					}

					functions.add(function);
				}
				else if(parent instanceof JSFile || function instanceof JSFunctionExpression)
				{
					final String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);
					if(qName != null)
					{
						Set<JSFunction> functions = jsFunctionsToProcess.get(qName);

						if(functions == null)
						{
							functions = new HashSet<>();
							jsFunctionsToProcess.put(qName, functions);
						}

						functions.add(function);
					}
				}
			}
			else if(el instanceof JSClass)
			{
				final JSClass clazz = (JSClass) el;
				if(!jsMethodsToProcess.containsKey(clazz))
				{
					jsMethodsToProcess.put(clazz, null);
				}
			}
		}

		for(Map.Entry<JSClass, Set<JSFunction>> entry : jsMethodsToProcess.entrySet())
		{
			ProgressManager.getInstance().checkCanceled();
			final JSClass clazz = entry.getKey();
			final Set<JSFunction> methods = entry.getValue();

			Query<JSClass> classQuery = JSClassSearch.searchClassInheritors(clazz, methods != null);

			classQuery.forEach(new Processor<JSClass>()
			{
				boolean addedClassMarker;
				final Set<JSFunction> methodsClone = methods == null || clazz.isInterface() ? null : new HashSet<>(methods);

				@Override
				public boolean process(final JSClass jsClass)
				{
					if(!addedClassMarker)
					{
						result.add(new LineMarkerInfo<>(clazz, clazz.getTextOffset(), AllIcons.Gutter.OverridenMethod, Pass.LINE_MARKERS,
								ourClassInheritorsTooltipProvider, ourClassInheritorsNavHandler));
						addedClassMarker = true;
					}

					if(methodsClone != null)
					{
						for(final Iterator<JSFunction> functionIterator = methodsClone.iterator(); functionIterator.hasNext(); )
						{
							final JSFunction function = functionIterator.next();

							final JSFunction byName = jsClass.findFunctionByNameAndKind(function.getName(), function.getKind());
							if(byName != null && !isNotApplicableForOverride(byName))
							{
								// TODO: more correct check for override
								function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);
								result.add(new LineMarkerInfo<>(function, function.getTextOffset(), AllIcons.Gutter.OverridenMethod, Pass.LINE_MARKERS,
										ourOverriddenFunctionsTooltipProvider, ourOverriddenFunctionsNavHandler));
								functionIterator.remove();
							}
						}
					}
					return methodsClone != null && !methodsClone.isEmpty();
				}
			});


			if(clazz.isInterface())
			{
				classQuery = JSClassSearch.searchInterfaceImplementations(clazz, false);

				if(classQuery.findFirst() != null)
				{
					result.add(new LineMarkerInfo<>(clazz, clazz.getTextOffset(), AllIcons.Gutter.ImplementedMethod, Pass.LINE_MARKERS,
							ourImplementedInterfacesTooltipProvider, ourInterfaceImplementationsNavHandler));
				}
			}

			if(methods == null)
			{
				continue;
			}

			for(final JSFunction function : methods)
			{

				if(clazz.isInterface())
				{
					final Query<JSFunction> query = JSFunctionsSearch.searchImplementingFunctions(function, false);
					if(query.findFirst() != null)
					{
						function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);
						result.add(new LineMarkerInfo<>(function, function.getTextOffset(), AllIcons.Gutter.ImplementedMethod, Pass.LINE_MARKERS,
								ourImplementingFunctionsTooltipProvider, ourImplementingFunctionsNavHandler));
					}
				}
				else
				{
					final JSAttributeList attributeList = function.getAttributeList();
					if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE))
					{
						continue;
					}

					final JSFunction implementedFunction = findImplementedFunction(function);
					if(implementedFunction != null)
					{
						function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);

						result.add(new LineMarkerInfo<>(function, function.getTextOffset(), AllIcons.Gutter.ImplementingMethod, Pass.LINE_MARKERS,
								jsFunction -> "Implementation of " + jsFunction.getName() + " in " + ((NavigationItem) implementedFunction.getParent()).getName(), new
								GutterIconNavigationHandler<JSFunction>()
						{
							@Override
							public void navigate(final MouseEvent e, final JSFunction elt)
							{
								final JSFunction implementedFunction = findImplementedFunction(elt);
								if(implementedFunction != null)
								{
									implementedFunction.navigate(true);
								}
							}
						}
						));
					}
				}
			}
		}
	}

	private static boolean isClass(final PsiElement element)
	{
		if(element instanceof JSClass)
		{
			return true;
		}
		if(element instanceof JSFile && element.getContext() != null)
		{
			return true;
		}
		return false;
	}

	@Nullable
	private static JSFunction findImplementedFunction(JSFunction implementingFunction)
	{
		PsiElement clazz = implementingFunction.getParent();
		if(!(clazz instanceof JSClass))
		{
			clazz = JSResolveUtil.getClassReferenceForXmlFromContext(clazz);
		}
		if(!(clazz instanceof JSClass))
		{
			return null;
		}
		final Ref<JSFunction> result = new Ref<>();
		JSResolveUtil.processInterfaceMethods((JSClass) clazz, new JSResolveUtil.CollectMethodsToImplementProcessor(implementingFunction.getName(),
				implementingFunction)
		{
			@Override
			protected boolean process(final ResolveProcessor processor)
			{
				result.set((JSFunction) processor.getResult());
				return false;
			}
		});
		return result.get();
	}

	private static boolean isNotApplicableForOverride(final JSFunction function)
	{
		final JSAttributeList attributeList = function.getAttributeList();

		return function.isConstructor() || (attributeList != null && (attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE ||
				attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) ||
				attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE)));
	}

	static class MyOverrideHandler implements JSResolveUtil.OverrideHandler
	{
		String className;

		@Override
		public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
		{
			this.className = className;
			return true;
		}
	}

	private abstract static class BasicGutterIconNavigationHandler<T extends PsiElement> implements GutterIconNavigationHandler<T>
	{
		@Override
		public void navigate(final MouseEvent e, final T elt)
		{
			final List<NavigatablePsiElement> navElements = new ArrayList<>();
			Query<T> elementQuery = search(elt);
			if(elementQuery == null)
			{
				return;
			}
			elementQuery.forEach(new Processor<T>()
			{
				@Override
				public boolean process(final T psiElement)
				{
					if(psiElement instanceof NavigatablePsiElement)
					{
						navElements.add((NavigatablePsiElement) psiElement);
					}
					return true;
				}
			});
			final NavigatablePsiElement[] methods = navElements.toArray(new NavigatablePsiElement[navElements.size()]);
			PsiElementListNavigator.openTargets(e, methods, getTitle(elt), "", new DefaultPsiElementCellRenderer());
		}

		protected abstract String getTitle(T elt);

		@Nullable
		protected abstract Query<T> search(T elt);
	}
}
