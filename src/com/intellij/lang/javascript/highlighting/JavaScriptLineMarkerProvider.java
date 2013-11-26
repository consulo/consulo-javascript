package com.intellij.lang.javascript.highlighting;

import gnu.trove.THashMap;
import gnu.trove.THashSet;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeHighlighting.Pass;
import com.intellij.codeInsight.daemon.GutterIconNavigationHandler;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.codeInsight.daemon.impl.PsiElementListNavigator;
import com.intellij.codeInsight.navigation.NavigationUtil;
import com.intellij.ide.util.DefaultPsiElementCellRenderer;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.index.JavaScriptSymbolProcessor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.search.JSClassSearch;
import com.intellij.lang.javascript.search.JSFunctionsSearch;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.ui.awt.RelativePoint;
import com.intellij.util.CollectionQuery;
import com.intellij.util.Function;
import com.intellij.util.Processor;
import com.intellij.util.Query;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 14, 2008
 *         Time: 11:43:47 PM
 */
public class JavaScriptLineMarkerProvider implements LineMarkerProvider
{
	@NonNls
	private static final String OVERRIDES_METHOD_IN = "overrides method in ";
	private static final Icon OVERRIDING_METHOD_ICON = IconLoader.getIcon("/gutter/overridingMethod.png");
	private static final Icon OVERRIDDEN_ICON = IconLoader.getIcon("/gutter/overridenMethod.png");
	private static final Icon IMPLEMENTED_ICON = IconLoader.getIcon("/gutter/implementedMethod.png");
	private static final Icon IMPLEMENTING_ICON = IconLoader.getIcon("/gutter/implementingMethod.png");

	private static final Function<JSClass, String> ourClassInheritorsTooltipProvider = new Function<JSClass, String>()
	{
		public String fun(final JSClass clazz)
		{
			return "Has subclasses";
		}
	};

	private static final Function<JSClass, String> ourImplementedInterfacesTooltipProvider = new Function<JSClass, String>()
	{
		public String fun(final JSClass clazz)
		{
			return "Has implementations";
		}
	};

	private static final Function<JSFunction, String> ourOverriddenFunctionsTooltipProvider = new Function<JSFunction, String>()
	{
		public String fun(final JSFunction psiElement)
		{
			return "Is overridden";
		}
	};

	private static final Function<JSFunction, String> ourImplementingFunctionsTooltipProvider = new Function<JSFunction, String>()
	{
		public String fun(final JSFunction psiElement)
		{
			return "Is implemented";
		}
	};

	private static final BasicGutterIconNavigationHandler<JSClass> ourClassInheritorsNavHandler = new BasicGutterIconNavigationHandler<JSClass>()
	{
		protected String getTitle(final JSClass elt)
		{
			return "Choose Subclass of " + elt.getName();
		}

		protected Query<JSClass> search(final JSClass elt)
		{
			return JSClassSearch.searchClassInheritors(elt, true);
		}
	};

	private static final BasicGutterIconNavigationHandler<JSClass> ourInterfaceImplementationsNavHandler = new
			BasicGutterIconNavigationHandler<JSClass>()
	{
		protected String getTitle(final JSClass elt)
		{
			return "Choose Implementation of " + elt.getName();
		}

		protected Query<JSClass> search(final JSClass elt)
		{
			return JSClassSearch.searchInterfaceImplementations(elt, true);
		}
	};

	private static final BasicGutterIconNavigationHandler<JSFunction> ourOverriddenFunctionsNavHandler = new
			BasicGutterIconNavigationHandler<JSFunction>()
	{
		protected String getTitle(final JSFunction elt)
		{
			return "Choose Overriden Function of " + elt.getName();
		}

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
			final ArrayList<JSFunction> result = new ArrayList<JSFunction>();
			MyNamespaceProcessor namespaceProcessor = new MyNamespaceProcessor(new THashSet<JSFunction>(Arrays.asList(elt)))
			{
				protected boolean doProcess(PsiElement elt)
				{
					if(elt instanceof JSNamedElementProxy)
					{
						elt = ((JSNamedElementProxy) elt).getElement();
					}
					if(elt instanceof JSProperty)
					{
						elt = ((JSProperty) elt).getValue();
					}
					if(!(elt instanceof JSFunction))
					{
						return true; // TODO: override can be caused by definitions, that generate classes
					}
					result.add((JSFunction) elt);
					return true;
				}
			};
			namespaceProcessor.processDescendantsOf(qName, elt.getProject());

			return new CollectionQuery<JSFunction>(result);
		}

		return new CollectionQuery<JSFunction>(Collections.<JSFunction>emptyList());
	}

	private static final BasicGutterIconNavigationHandler<JSFunction> ourImplementingFunctionsNavHandler = new
			BasicGutterIconNavigationHandler<JSFunction>()
	{
		protected String getTitle(final JSFunction elt)
		{
			return "Choose Implementation of " + elt.getName();
		}

		protected Query<JSFunction> search(final JSFunction elt)
		{
			return JSFunctionsSearch.searchImplementingFunctions(elt, true);
		}
	};

	public static Key<Boolean> ourParticipatesInHierarchyKey = Key.create("js.named.item.participates.in.hierarchy");

	public LineMarkerInfo getLineMarkerInfo(final PsiElement element)
	{
		if(element instanceof JSReferenceExpression)
		{
			final ResolveResult[] results = ((JSReferenceExpression) element).multiResolve(false);
			boolean isStatic = false;
			boolean isMethod = false;
			boolean isFunction = false;
			boolean isVariable = false;
			boolean isField = false;
			TextAttributesKey type = null;
			@NonNls String text = null;

			for(ResolveResult r : results)
			{
				final PsiElement resolve = r.getElement();

				if(resolve instanceof JSNamedElementProxy)
				{
					final JSNamedElementProxy elementProxy = (JSNamedElementProxy) resolve;
					final JSNamedElementProxy.NamedItemType namedItemType = elementProxy.getType();

					if(namedItemType == JSNamedElementProxy.NamedItemType.AttributeValue)
					{
						type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
						text = "field";
					}
					else
					{
						isStatic |= elementProxy.hasProperty(JSNamedElementProxy.Property.Static);

						isMethod |= (namedItemType == JSNamedElementProxy.NamedItemType.MemberFunction || namedItemType == JSNamedElementProxy
								.NamedItemType.FunctionProperty);
						isFunction |= namedItemType == JSNamedElementProxy.NamedItemType.Function;
						isVariable |= namedItemType == JSNamedElementProxy.NamedItemType.Variable;
						isField |= namedItemType == JSNamedElementProxy.NamedItemType.Definition || namedItemType == JSNamedElementProxy
								.NamedItemType.Property;

						if(namedItemType == JSNamedElementProxy.NamedItemType.FunctionExpression)
						{
							final JSNamespace namespace = elementProxy.getNamespace();

							if(namespace.getNameId() == -1)
							{
								isFunction = true;
							}
							else
							{
								isMethod = true;
							}
						}
					}
				}
				else if(resolve instanceof JSAttributeListOwner)
				{
					if(resolve instanceof JSVariable)
					{
						return buildHighlightForVariable(resolve, element);
					}

					final JSAttributeList attributeList = ((JSAttributeListOwner) resolve).getAttributeList();

					if(attributeList != null)
					{
						isStatic |= attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
					}

					isMethod = resolve instanceof JSFunction;
					if(isMethod && !isClass(resolve.getParent()))
					{
						isMethod = false;
						isFunction = true;
					}
				}
				else if(resolve instanceof JSDefinitionExpression)
				{
					final PsiElement parent = resolve.getParent();
					if(parent instanceof JSAssignmentExpression)
					{
						final JSExpression jsExpression = ((JSAssignmentExpression) parent).getROperand();
						if(jsExpression instanceof JSFunctionExpression)
						{
							isMethod = true;
						}
						else
						{
							isField = true;
						}
					}
				}
				else if(resolve instanceof JSProperty)
				{
					final JSExpression expression = ((JSProperty) resolve).getValue();

					if(expression instanceof JSFunctionExpression)
					{
						isMethod = true;
					}
					else
					{
						isField = true;
					}
				}
			}

			if(isMethod)
			{
				if(isStatic)
				{
					type = JSHighlighter.JS_STATIC_MEMBER_FUNCTION;
					if(myUnitTestMode)
					{
						text = "static method";
					}
				}
				else
				{
					type = JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION;
					if(myUnitTestMode)
					{
						text = INSTANCE_METHOD;
					}
				}
			}
			else if(isFunction)
			{
				type = JSHighlighter.JS_GLOBAL_FUNCTION;
				if(myUnitTestMode)
				{
					text = "global function";
				}
			}
			else if(isVariable)
			{
				type = JSHighlighter.JS_GLOBAL_VARIABLE;
				if(myUnitTestMode)
				{
					text = "global variable";
				}
			}
			else if(isField)
			{
				type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
				if(myUnitTestMode)
				{
					text = INSTANCE_FIELD;
				}
			}

			return createLineMarker(element, type, text);
		}
		else if(element instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) element;
			function.putUserData(ourParticipatesInHierarchyKey, null);
			if(function.findNameIdentifier() == null)
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

					return new LineMarkerInfo<JSFunction>(function, function.findNameIdentifier().getTextRange().getStartOffset(),
							OVERRIDING_METHOD_ICON, Pass.UPDATE_ALL, new Function<JSFunction, String>()
					{
						public String fun(final JSFunction psiElement)
						{
							return OVERRIDES_METHOD_IN + overrideHandler.className;
						}
					}, new GutterIconNavigationHandler<JSFunction>()
					{
						public void navigate(final MouseEvent e, final JSFunction elt)
						{
							final Set<NavigationItem> results = new THashSet<NavigationItem>();
							JSResolveUtil.iterateType(function, parentNode1, typeName, new JSResolveUtil.OverrideHandler()
							{
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
								NavigationUtil.getPsiElementPopup(results.toArray(new PsiElement[results.size()]), "Choose super class or interface").show(new RelativePoint(e));
							}
						}
					}
					);
				}
			}
		}
		else if(element instanceof JSProperty)
		{
			final JSProperty property = (JSProperty) element;

			final JSExpression expression = property.getValue();
			TextAttributesKey type = null;
			@NonNls String text = null;

			if(expression instanceof JSFunctionExpression)
			{
				type = JSHighlighter.JS_INSTANCE_MEMBER_FUNCTION;
				if(myUnitTestMode)
				{
					text = INSTANCE_METHOD;
				}
			}
			else
			{
				type = JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
				if(myUnitTestMode)
				{
					text = INSTANCE_FIELD;
				}
			}
			return createLineMarker(property, type, text);
		}
		else if(element instanceof JSAttribute)
		{
			return createLineMarker(element, JSHighlighter.JS_METADATA, myUnitTestMode ? "attribute" : null);
		}

		return null;
	}

	public void collectSlowLineMarkers(final List<PsiElement> elements, final Collection<LineMarkerInfo> result)
	{
		final Map<String, Set<JSFunction>> jsFunctionsToProcess = new THashMap<String, Set<JSFunction>>();
		final Map<JSClass, Set<JSFunction>> jsMethodsToProcess = new THashMap<JSClass, Set<JSFunction>>();

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
						functions = new THashSet<JSFunction>();
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
							functions = new THashSet<JSFunction>();
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

		for(Map.Entry<String, Set<JSFunction>> entry : jsFunctionsToProcess.entrySet())
		{
			ProgressManager.getInstance().checkCanceled();

			MyNamespaceProcessor processor = new MyNamespaceProcessor(entry.getValue())
			{
				protected boolean doProcess(final PsiElement elt)
				{
					function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);
					result.add(new LineMarkerInfo<JSFunction>(function, function.getTextOffset(), OVERRIDDEN_ICON, Pass.UPDATE_OVERRIDEN_MARKERS,
							ourOverriddenFunctionsTooltipProvider, ourOverriddenFunctionsNavHandler));
					return false;
				}
			};
			processor.processDescendantsOf(entry.getKey(), elements.get(0).getProject());
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
				final Set<JSFunction> methodsClone = methods == null || clazz.isInterface() ? null : new THashSet<JSFunction>(methods);

				public boolean process(final JSClass jsClass)
				{
					if(!addedClassMarker)
					{
						result.add(new LineMarkerInfo<JSClass>(clazz, clazz.getTextOffset(), OVERRIDDEN_ICON, Pass.UPDATE_OVERRIDEN_MARKERS,
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
								result.add(new LineMarkerInfo<JSFunction>(function, function.getTextOffset(), OVERRIDDEN_ICON,
										Pass.UPDATE_OVERRIDEN_MARKERS, ourOverriddenFunctionsTooltipProvider, ourOverriddenFunctionsNavHandler));
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
					result.add(new LineMarkerInfo<JSClass>(clazz, clazz.getTextOffset(), IMPLEMENTED_ICON, Pass.UPDATE_OVERRIDEN_MARKERS,
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
						result.add(new LineMarkerInfo<JSFunction>(function, function.getTextOffset(), IMPLEMENTED_ICON,
								Pass.UPDATE_OVERRIDEN_MARKERS, ourImplementingFunctionsTooltipProvider, ourImplementingFunctionsNavHandler));
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

						result.add(new LineMarkerInfo<JSFunction>(function, function.getTextOffset(), IMPLEMENTING_ICON,
								Pass.UPDATE_OVERRIDEN_MARKERS, new Function<JSFunction, String>()
						{
							public String fun(final JSFunction jsFunction)
							{
								return "Implementation of " + jsFunction.getName() + " in " + ((NavigationItem) implementedFunction.getParent())
										.getName();
							}
						}, new GutterIconNavigationHandler<JSFunction>()
						{
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
		final Ref<JSFunction> result = new Ref<JSFunction>();
		JSResolveUtil.processInterfaceMethods((JSClass) clazz, new JSResolveUtil.CollectMethodsToImplementProcessor(implementingFunction.getName(),
				implementingFunction)
		{
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

		public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
		{
			this.className = className;
			return true;
		}
	}

	private abstract static class BasicGutterIconNavigationHandler<T extends PsiElement> implements GutterIconNavigationHandler<T>
	{
		public void navigate(final MouseEvent e, final T elt)
		{
			final List<NavigatablePsiElement> navElements = new ArrayList<NavigatablePsiElement>();
			Query<T> elementQuery = search(elt);
			if(elementQuery == null)
			{
				return;
			}
			elementQuery.forEach(new Processor<T>()
			{
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

	private abstract static class MyNamespaceProcessor extends JavaScriptSymbolProcessor.DefaultSymbolProcessor implements JSTypeEvaluateManager
			.NamespaceProcessor
	{
		private final Set<JSFunction> myFunctions;
		private LinkedList<String> myDescendants = new LinkedList<String>();
		private final Set<String> myProcessed = new THashSet<String>();
		protected JSFunction function;
		private int myDescendantNameId;

		public MyNamespaceProcessor(Set<JSFunction> functions)
		{
			myFunctions = functions;
		}

		public boolean process(final JSNamespace superNs)
		{
			JavaScriptIndex index = JavaScriptIndex.getInstance(myFunctions.iterator().next().getProject());

			String qName = superNs.getQualifiedName(index);
			if(myProcessed.contains(qName))
			{
				return true;
			}
			myProcessed.add(qName);

			myDescendants.add(qName);
			boolean result = false;
			Iterator<JSFunction> functionIterator = myFunctions.iterator();

			while(functionIterator.hasNext())
			{
				function = functionIterator.next();
				JSNamespace ns = superNs;
				final String funName = function.getName();

				if(function instanceof JSFunctionExpression && funName != null && funName.length() > 0 && (Character.isLowerCase(funName.charAt(0))
						|| '_' == funName.charAt(0)))
				{
					myDescendantNameId = index.getIndexOf(funName);
				}
				else
				{
					ns = ns.getParent();
					myDescendantNameId = superNs.getNameId();
				}

				final boolean b = ns.processDeclarations(this);
				if(!b)
				{
					functionIterator.remove();
				}
				result |= b;
			}

			return result;
		}

		protected boolean process(PsiElement namedElement, final JSNamespace namespace)
		{
			return doProcess(namedElement);
		}

		protected abstract boolean doProcess(final PsiElement elt);

		public PsiFile getBaseFile()
		{
			return null;
		}

		public int getRequiredNameId()
		{
			return myDescendantNameId;
		}

		public void processDescendantsOf(String qName, Project project)
		{
			ProgressManager progressManager = ProgressManager.getInstance();
			progressManager.checkCanceled();
			boolean b = JSTypeEvaluateManager.getInstance(project).iterateSubclasses(qName, this);

			while(b && myDescendants.size() > 0)
			{
				progressManager.checkCanceled();
				b = JSTypeEvaluateManager.getInstance(project).iterateSubclasses(myDescendants.removeFirst(), this);
			}
		}
	}
}
