package com.intellij.lang.javascript.psi.resolve;

import gnu.trove.THashMap;
import gnu.trove.THashSet;

import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexImportSupport;
import com.intellij.lang.javascript.flex.JSResolveHelper;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassBase;
import com.intellij.lang.javascript.psi.impl.JSStubElementImpl;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.UserDataCache;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.Processor;
import com.intellij.util.containers.ConcurrentHashMap;
import com.intellij.xml.XmlElementDescriptor;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 29, 2008
 *         Time: 6:52:42 PM
 */
public class JSImportHandlingUtil
{
	public static final Key<CachedValue<Map<String, Object>>> ourImportListCache = Key.create("js.import.list.cache");
	public static final UserDataCache<CachedValue<Map<String, Object>>, PsiElement, Object> myImportListCache = new ImportListDataCache();
	public static final Key<CachedValue<Map<String, JSImportedElementResolveResult>>> ourImportResolveCache = Key.create("js.import.resolve");
	public static final UserDataCache<CachedValue<Map<String, JSImportedElementResolveResult>>, PsiElement,
			Object> myImportResolveCache = new UserDataCache<CachedValue<Map<String, JSImportedElementResolveResult>>, PsiElement, Object>()
	{
		protected CachedValue<Map<String, JSImportedElementResolveResult>> compute(final PsiElement psiElement, final Object p)
		{
			return CachedValuesManager.getManager(psiElement.getProject()).createCachedValue(new CachedValueProvider<Map<String,
					JSImportedElementResolveResult>>()
			{
				public Result<Map<String, JSImportedElementResolveResult>> compute()
				{
					return new Result<Map<String, JSImportedElementResolveResult>>(new ConcurrentHashMap<String, JSImportedElementResolveResult>(),
							PsiModificationTracker.MODIFICATION_COUNT);
				}
			}, false);
		}
	};

	public static String resolveTypeName(final String _str, @NotNull PsiElement context)
	{
		final JSImportedElementResolveResult resolveResult = _resolveTypeName(_str, context);
		if(resolveResult == null)
		{
			return _str;
		}
		return resolveResult.qualifiedName;
	}

	// TODO _str should be JSReferenceExpression for caching!
	private static JSImportedElementResolveResult _resolveTypeName(final String _name, @NotNull PsiElement context)
	{
		String name = _name;
		if(name == null)
		{
			return null;
		}
		final int i = name.indexOf('<');
		String signature = null;

		if(i != -1)
		{
			final int index = name.lastIndexOf('.', i);
			if(index == -1)
			{
				return null;
			}
			signature = name.substring(index);
			name = name.substring(0, index);
		}

		if(name.indexOf('.') != -1)
		{
			return null;
		}

		final Ref<JSImportedElementResolveResult> resultRef = new Ref<JSImportedElementResolveResult>();

		final String name1 = name;
		JSResolveUtil.walkOverStructure(context, new Processor<PsiNamedElement>()
		{
			public boolean process(PsiNamedElement context)
			{
				JSImportedElementResolveResult resolved = null;

				if(context instanceof XmlBackedJSClassImpl)
				{ // reference list in mxml
					XmlTag rootTag = ((XmlBackedJSClassImpl) context).getParent();
					final XmlElementDescriptor descriptor = rootTag != null ? rootTag.getDescriptor() : null;
					PsiElement element = descriptor != null ? descriptor.getDeclaration() : null;
					if(element instanceof XmlFile)
					{
						element = XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) element);
					}

					final String s = element instanceof JSNamedElementProxy ? ((JSNamedElementProxy) element).getQualifiedName() : element instanceof JSClass ? (
							(JSClass) element).getQualifiedName() : rootTag.getLocalName();
					resolved = new JSImportedElementResolveResult(s);
				}
				else if(context instanceof JSQualifiedNamedElement)
				{
					if(context instanceof JSClass && name1.equals(context.getName()))
					{
						resolved = new JSImportedElementResolveResult(((JSQualifiedNamedElement) context).getQualifiedName());
					}
					else
					{
						resolved = resolveTypeNameUsingImports(name1, context);

						if(resolved == null)
						{
							final String qName = ((JSQualifiedNamedElement) context).getQualifiedName();
							final String packageName = qName != null ? context instanceof JSPackageStatement ? qName + "." : qName.substring(0,
									qName.lastIndexOf('.') + 1) : "";

							if(packageName.length() != 0)
							{
								final PsiElement byQName = JSClassBase.findClassFromNamespace(packageName + name1, context);

								if(byQName instanceof JSQualifiedNamedElement)
								{
									resolved = new JSImportedElementResolveResult(((JSQualifiedNamedElement) byQName).getQualifiedName());
								}
							}
						}
					}
				}
				else
				{
					resolved = resolveTypeNameUsingImports(name1, context);
					PsiElement contextOfContext;

					if(resolved == null && context instanceof JSFile && (contextOfContext = context.getContext()) != null)
					{
						PsiFile containingFile = contextOfContext.getContainingFile();
						XmlBackedJSClassImpl clazz = containingFile instanceof XmlFile ? (XmlBackedJSClassImpl) XmlBackedJSClassImpl.getXmlBackedClass((XmlFile)
								containingFile) : null;

						if(clazz != null)
						{
							ResolveProcessor r = new ResolveProcessor(name1);
							if(!clazz.processComponentNames(r))
							{
								PsiElement resultFromProcessor = r.getResult();
								JSClass clazzFromComponent = resultFromProcessor instanceof JSClass ? (JSClass) resultFromProcessor : null;

								if(clazzFromComponent != null)
								{
									resolved = new JSImportedElementResolveResult(clazzFromComponent.getQualifiedName(), clazz, null);
								}
							}
						}
					}
				}

				if(resolved != null)
				{
					resultRef.set(resolved);
					return false;
				}

				if(context instanceof JSPackageStatement)
				{
					return false;
				}
				return true;
			}
		});

		JSImportedElementResolveResult result = resultRef.get();

		if(signature != null && result != null)
		{
			result = result.appendSignature(signature);
		}
		return result;
	}

	private static JSQualifiedNamedElement resolveTypeNameInTheSamePackage(final String str, final PsiElement context)
	{
		final String packageQualifierText = JSResolveUtil.findPackageStatementQualifier(context);
		final String candidateText = packageQualifierText != null ? packageQualifierText + "." + str : str;

		PsiElement byQName = JSClassBase.findClassFromNamespace(candidateText, context);
		if(byQName instanceof JSQualifiedNamedElement)
		{
			return (JSQualifiedNamedElement) byQName;
		}

		if(packageQualifierText != null)
		{
			byQName = JSClassBase.findClassFromNamespace(str, context);
			if(byQName instanceof JSQualifiedNamedElement)
			{
				return (JSQualifiedNamedElement) byQName;
			}
		}

		return null;
	}

	public static
	@Nullable
	JSImportedElementResolveResult resolveTypeNameUsingImports(@NotNull final JSReferenceExpression expr)
	{
		if(expr.getQualifier() != null)
		{
			return null;
		}
		if(JSResolveUtil.referenceExpressionShouldBeQualified(expr))
		{
			return null;
		}
		if(JavaScriptIndex.getInstance(expr.getProject()).inUpdateState())
		{
			return null;
		}
		if(expr.getReferencedName() == null)
		{
			return null;
		}

		return _resolveTypeName(expr.getText(), expr);
	}

	private static
	@Nullable
	JSImportedElementResolveResult resolveTypeNameUsingImports(final @NotNull String referencedName, PsiNamedElement parent)
	{
		final Map<String, JSImportedElementResolveResult> map = myImportResolveCache.get(ourImportResolveCache, parent, null).getValue();
		JSImportedElementResolveResult result = map.get(referencedName);

		if(result == null)
		{
			result = resolveTypeNameUsingImportsInner(referencedName, parent);
			map.put(referencedName, result != null ? result : JSImportedElementResolveResult.EMPTY_RESULT);
		}

		return result != JSImportedElementResolveResult.EMPTY_RESULT ? result : null;
	}

	private static JSImportedElementResolveResult resolveTypeNameUsingImportsInner(final String referencedName, final PsiNamedElement parent)
	{
		final Map<String, Object> value = myImportListCache.get(ourImportListCache, parent, null).getValue();
		JSImportedElementResolveResult expression = FlexImportSupport.tryFindInMap(referencedName, parent, value);
		if(expression != null)
		{
			return expression;
		}

		if(parent instanceof JSPackageStatement)
		{
			return checkTheSamePackageOrGlobal(referencedName, parent);
		}
		else if(parent instanceof JSFile && parent.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4))
		{
			final PsiElement element = JSResolveUtil.getClassReferenceForXmlFromContext(parent);

			if(element instanceof XmlBackedJSClassImpl)
			{
				final ResolveProcessor processor = new ResolveProcessor(referencedName);
				final boolean b = ((XmlBackedJSClassImpl) element).doImportFromScripts(processor, parent);

				if(!b)
				{
					final JSQualifiedNamedElement jsClass = (JSQualifiedNamedElement) processor.getResult();
					return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, processor.getImportUsed());
				}

				JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, element);

				if(jsClass == null)
				{
					final JSClass parentClass = (JSClass) element;
					final JSClass[] classes = parentClass.getSuperClasses();

					if(classes != null && classes.length > 0 && referencedName.equals(classes[0].getName()))
					{
						jsClass = classes[0];
					}
				}

				if(jsClass != null)
				{
					return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
				}
			}
			else
			{
				final JSImportedElementResolveResult resolveResult = checkTheSamePackageOrGlobal(referencedName, parent);
				if(resolveResult != null)
				{
					return resolveResult;
				}
			}

			expression = FlexImportSupport.resolveTypeNameUsingImplicitImports(referencedName, (JSFile) parent);
			if(expression != null)
			{
				return expression;
			}
		}
		else if(parent instanceof XmlBackedJSClassImpl)
		{
			JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, parent);
			if(jsClass != null)
			{
				return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
			}

			final Ref<JSImportedElementResolveResult> result = new Ref<JSImportedElementResolveResult>();
			processInlineComponentsInScope((XmlBackedJSClassImpl) parent, new Processor<XmlBackedJSClassImpl>()
			{
				public boolean process(XmlBackedJSClassImpl inlineComponent)
				{
					if(referencedName.equals(inlineComponent.getExplicitName()))
					{
						result.set(new JSImportedElementResolveResult(inlineComponent.getQualifiedName(), inlineComponent, null));
						return false;
					}
					return true;
				}
			});
			if(!result.isNull())
			{
				return result.get();
			}
		}

		return null;
	}

	private static boolean processInlineComponentsInScope(XmlBackedJSClassImpl context, Processor<XmlBackedJSClassImpl> processor)
	{
		XmlTag rootTag = ((XmlFile) context.getContainingFile()).getDocument().getRootTag();
		boolean recursive = XmlBackedJSClassImpl.isInlineComponentTag(context.getParent());
		for(XmlBackedJSClassImpl inlineComponent : XmlBackedJSClassImpl.getChildInlineComponents(rootTag, recursive))
		{
			if(!processor.process(inlineComponent))
			{
				return false;
			}
		}
		return true;
	}

	private static JSImportedElementResolveResult checkTheSamePackageOrGlobal(final String referencedName, final PsiNamedElement parent)
	{
		final JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, parent);

		if(jsClass != null)
		{
			return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
		}
		return null;
	}

	public static boolean tryResolveImports(final PsiScopeProcessor processor, PsiNamedElement parent, @NotNull PsiElement place)
	{
		if(!isAdequatePlaceForImport(parent, place))
		{
			return true;
		}

		return !importClass(processor, parent);
	}

	public static boolean importClass(final PsiScopeProcessor processor, final PsiNamedElement parent)
	{
		if(processor instanceof ResolveProcessor && ((ResolveProcessor) processor).isLocalResolve())
		{
			return false;
		}
		ResolveProcessor resolveProcessor = (ResolveProcessor) processor;
		final String s = resolveProcessor.getName();

		if(s != null)
		{
			if(resolveProcessor.specificallyAskingToResolveQualifiedNames())
			{
				final Map<String, Object> value = myImportListCache.get(ourImportListCache, parent, null).getValue();
				JSImportedElementResolveResult resolveResult = FlexImportSupport.tryFindInMap(s, parent, value, resolveProcessor.getQualifiedNameToImport());
				if(dispatchResult(resolveResult, processor))
				{
					return true;
				}
			}

			final JSImportedElementResolveResult expression = resolveTypeNameUsingImports(s, parent);

			if(dispatchResult(expression, processor))
			{
				return true;
			}
		}
		else
		{
			if(parent instanceof XmlBackedJSClassImpl)
			{
				if(!processInlineComponentsInScope((XmlBackedJSClassImpl) parent, new Processor<XmlBackedJSClassImpl>()
				{
					public boolean process(XmlBackedJSClassImpl inlineComponent)
					{
						return processor.execute(inlineComponent, ResolveState.initial());
					}
				}))
				{
					return false;
				}
			}
			final String packageQualifierText = JSResolveUtil.findPackageStatementQualifier(parent);
			importClassViaHelper(processor, parent, packageQualifierText);
		}

		return false;
	}

	private static boolean dispatchResult(JSImportedElementResolveResult expression, PsiScopeProcessor processor)
	{
		if(expression != null)
		{
			final PsiElement element = expression.resolvedElement;

			if(element != null)
			{
				ResolveState state = ResolveState.initial();
				if(expression.importStatement != null)
				{
					state = state.put(ResolveProcessor.IMPORT_KEY, expression.importStatement);
				}
				return !processor.execute(element, state);
			}
		}

		return false;
	}

	public static void importClassViaHelper(final PsiScopeProcessor processor, final PsiNamedElement file, final String packageQualifierText)
	{
		for(JSResolveHelper helper : Extensions.getExtensions(JSResolveHelper.EP_NAME))
		{
			helper.importClass(processor, file, packageQualifierText);
		}
	}

	public static boolean isAdequatePlaceForImport(final PsiNamedElement parent, @NotNull PsiElement place)
	{
		if(parent instanceof JSFile && !parent.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4))
		{
			return false;
		}

		if(place instanceof JSReferenceExpression)
		{
			final PsiElement placeParent = place.getParent();

			if(placeParent instanceof JSReferenceExpression)
			{
				final PsiElement currentParent = JSResolveUtil.getTopReferenceParent(placeParent);

				if(JSResolveUtil.isSelfReference(currentParent, place) ||
						//currentParent instanceof JSDefinitionExpression ||
						currentParent instanceof JSReferenceList)
				{
					return false;
				}
			}
		}
		else if(place instanceof JSDocTagValue)
		{
			// further conditions to come
		}
		else
		{
			if(!(place instanceof JSFile))
			{
				return false;
			}
		}

		return true;
	}

	private static class ImportListDataCache extends UserDataCache<CachedValue<Map<String, Object>>, PsiElement, Object>
	{
		protected final CachedValue<Map<String, Object>> compute(final PsiElement owner, Object o)
		{
			return CachedValuesManager.getManager(owner.getProject()).createCachedValue(new CachedValueProvider<Map<String, Object>>()
			{
				public Result<Map<String, Object>> compute()
				{
					final Map<String, Object> result = new THashMap<String, Object>();
					collect(result, owner, null);
					return new Result<Map<String, Object>>(result, owner);
				}
			}, false);
		}

		private static void collect(final Map<String, Object> result, final PsiElement owner, Set<PsiFile> visitedIncludes)
		{
			PsiElement[] children = PsiElement.EMPTY_ARRAY;

			if(owner instanceof JSIncludeDirective)
			{
				final PsiFile file = ((JSIncludeDirective) owner).resolveFile();
				if(file != null && (visitedIncludes == null || !visitedIncludes.contains(file)))
				{
					if(visitedIncludes == null)
					{
						visitedIncludes = new THashSet<PsiFile>();
					}
					visitedIncludes.add(file);
					children = JSResolveUtil.getSourceElements(file);
				}
			}
			else if(owner instanceof JSFile || owner instanceof JSStubElementImpl)
			{
				children = JSResolveUtil.getSourceElements(owner);
			}
			else
			{
				children = owner.getChildren();
			}

			for(PsiElement c : children)
			{
				if(c instanceof JSImportStatement)
				{
					final JSImportStatement s = ((JSImportStatement) c);

					if(s.getImportText() != null)
					{
						FlexImportSupport.appendToMap(result, s);
					}
				}
				else if(!(c instanceof JSPackageStatement) && !(c instanceof JSFunction))
				{
					collect(result, c, visitedIncludes);
				}
			}
		}
	}
}
