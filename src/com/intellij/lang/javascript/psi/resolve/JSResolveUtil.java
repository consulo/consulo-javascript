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
import gnu.trove.TIntArrayList;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TIntObjectIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.extapi.psi.PsiElementBase;
import com.intellij.ide.highlighter.XmlFileType;
import com.intellij.injected.editor.VirtualFileWindow;
import com.intellij.javascript.documentation.JSDocumentationProvider;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.JSResolveHelper;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.index.JSPackage;
import com.intellij.lang.javascript.index.JSPackageIndex;
import com.intellij.lang.javascript.index.JSPackageIndexInfo;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.index.JavaScriptSymbolProcessor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lang.javascript.psi.impl.JSPackageWrapper;
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.impl.JSStubElementImpl;
import com.intellij.lang.javascript.psi.stubs.JSNameIndex;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedElementIndex;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.UserDataCache;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.light.LightElement;
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.ParameterizedCachedValue;
import com.intellij.psi.util.ParameterizedCachedValueProvider;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlDocument;
import com.intellij.psi.xml.XmlElement;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlTagChild;
import com.intellij.psi.xml.XmlText;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.text.StringTokenizer;
import com.intellij.xml.XmlElementDescriptor;

/**
 * @author max, maxim.mossienko
 */
public class JSResolveUtil
{
	private static final Key<CachedValue<TIntObjectHashMap<Object>>> MY_CACHED_STATEMENTS = Key.create("JS.RelevantStatements");
	private static UserDataCache<CachedValue<TIntObjectHashMap<Object>>, JSElement, Object> ourCachedDefsCache = new RelevantDefsUserDataCache();
	@NonNls
	public static final String PROTOTYPE_FIELD_NAME = "prototype";

	private static final Key<GlobalSearchScope> MY_SCOPE_KEY = Key.create("default.scope");
	public static final Key<Boolean> IMPLICIT_JS_FILES_KEY = Key.create("implicit.js.files");
	@NonNls
	private static final String ARRAY_TYPE_NAME = "Array";
	@NonNls
	private static final String ARGUMENTS_TYPE_NAME = "Arguments";

	public static void processInjectedFileForTag(final @NotNull XmlTag tag, @NotNull JSInjectedFilesVisitor visitor)
	{

		for(XmlTagChild child : tag.getValue().getChildren())
		{
			if(child instanceof XmlText)
			{
				InjectedLanguageUtil.enumerate(child, visitor);
			}
		}
	}

	public static String findPackageForMxml(final PsiElement expression)
	{
		String s = null;
		final PsiFile containingFile = expression.getContainingFile();

		if(containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4 && containingFile.getContext() != null)
		{
			final PsiFile contextContainigFile = containingFile.getContext().getContainingFile();
			VirtualFile file = contextContainigFile.getVirtualFile();
			if(file == null && contextContainigFile.getOriginalFile() != null)
			{
				file = contextContainigFile.getOriginalFile().getVirtualFile();
			}

			s = getExpectedPackageNameFromFile(file, containingFile.getProject(), true);
		}
		return s;
	}

	public static String getExpectedPackageNameFromFile(VirtualFile file, Project project, boolean allowEvaluationFromContextRoot)
	{
		final ProjectFileIndex projectFileIndex = ProjectRootManager.getInstance(project).getFileIndex();
		final Module moduleForFile = file != null ? projectFileIndex.getModuleForFile(file) : null;

		if(moduleForFile != null)
		{
			if(file instanceof VirtualFileWindow)
			{
				file = ((VirtualFileWindow) file).getDelegate();
			}
			VirtualFile rootForFile = projectFileIndex.getSourceRootForFile(file);
			if(rootForFile == null && allowEvaluationFromContextRoot)
			{
				rootForFile = projectFileIndex.getContentRootForFile(file);
			}

			if(rootForFile != null)
			{
				return VfsUtil.getRelativePath(file.isDirectory() ? file : file.getParent(), rootForFile, '.');
			}
		}
		return null;
	}

	public static void processInterfaceMethods(final JSClass clazz, final CollectMethodsToImplementProcessor implementedMethodProcessor)
	{
		clazz.processDeclarations(implementedMethodProcessor, ResolveState.initial(), clazz, clazz);
	}

	public static String getExpressionType(final JSExpression expression, final PsiFile containingFile)
	{
		String type = getQualifiedExpressionType(expression, containingFile);

		return getShortenedType(type, expression);
	}

	public static String getShortenedType(String type, final PsiElement context)
	{
		if(type == null)
		{
			type = "*";
		}
		else
		{
			String shortName = getShortTypeName(type);
			final String expr = JSImportHandlingUtil.resolveTypeName(shortName, context);
			if(expr != null && !expr.equals(shortName))
			{
				type = shortName;
			}
		}
		return type;
	}

	public static String getQualifiedExpressionType(final JSExpression expression, final PsiFile containingFile)
	{
		String type = null;

		if(expression != null)
		{
			final BaseJSSymbolProcessor.SimpleTypeProcessor processor = new BaseJSSymbolProcessor.SimpleTypeProcessor(true);
			BaseJSSymbolProcessor.doEvalForExpr(expression, containingFile, processor);
			type = processor.getType();
		}

		return type;
	}

	private static String getShortTypeName(String type)
	{
		String str = type;

		final int i = str.indexOf('<');
		String signature = null;

		if(i != -1)
		{
			final int index = str.lastIndexOf('.', i);
			if(index == -1)
			{
				return type;
			}
			signature = str.substring(index);
			str = str.substring(0, index);
		}

		final int i2 = str.lastIndexOf('.');
		if(i2 != -1)
		{
			str = str.substring(i2 + 1);
		}

		return str + (signature != null ? signature : "");
	}

	// TODO remove this method, call generic one processing includes as well (like findParent())
	public static PsiElement getClassReferenceForXmlFromContext(PsiElement parent)
	{
		final PsiElement context = parent != null ? parent.getContext() : null;
		if(context instanceof XmlElement && context.getContainingFile() instanceof XmlFile)
		{
			return XmlBackedJSClassImpl.getContainingComponent((XmlElement) context);
		}
		return parent;
	}

	@Nullable
	public static XmlBackedJSClassImpl getXmlBackedClass(final JSFile injectedJsFile)
	{
		final PsiElement context = injectedJsFile.getContext();
		if(context instanceof XmlAttributeValue || context instanceof XmlText)
		{
			return XmlBackedJSClassImpl.getContainingComponent((XmlElement) context);
		}
		return null;
	}


	public static <T extends JSNamedElement & JSAttributeListOwner> SearchScope findUseScope(final T jsVariableBase)
	{
		PsiElement element = PsiTreeUtil.getParentOfType(jsVariableBase, JSFunction.class, JSCatchBlock.class, JSClass.class, JSObjectLiteralExpression.class, JSFile.class);
		PsiElement scopeElement = element;

		if(element instanceof JSFile)
		{
			final PsiElement xmlFromContext = getClassReferenceForXmlFromContext(element);

			if(xmlFromContext != element)
			{
				element = xmlFromContext;
				scopeElement = element.getContainingFile();
			}
			else
			{
				element = null;
			}
		}

		if(element != null)
		{
			if(element instanceof JSFunction)
			{
				final PsiElement elt = JSDocumentationUtils.findDocComment(JSDocumentationProvider.findElementForWhichPreviousCommentWillBeSearched(element));
				if(elt instanceof PsiComment)
				{
					return new LocalSearchScope(new PsiElement[]{
							elt,
							element
					});
				}
			}

			if(element instanceof JSClass)
			{
				final JSAttributeList attributeList = jsVariableBase.getAttributeList();

				if(attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PRIVATE)
				{
					return ((JSStubElementImpl) jsVariableBase).getDefaultUseScope();
				}
			}
			return new LocalSearchScope(scopeElement);
		}
		return ((JSStubElementImpl) jsVariableBase).getDefaultUseScope();
	}

	public static final boolean isAssignableType(@NonNls final String expectedType, @NonNls String type, PsiElement context)
	{
		if((expectedType != null && hasMultipleOccurenceDelimiters(expectedType)) || (type != null && hasMultipleOccurenceDelimiters(type)))
		{
			StringTokenizer expectedTypeIterator = new StringTokenizer(expectedType != null ? expectedType : "", JSType.COMMENT_DELIMITERS);

			while(expectedTypeIterator.hasMoreElements())
			{
				String primitiveExpectedType = expectedTypeIterator.nextToken().trim();
				StringTokenizer typeIterator = new StringTokenizer(type != null ? type : "", JSType.COMMENT_DELIMITERS);

				while(typeIterator.hasMoreElements())
				{
					String primitiveType = typeIterator.nextToken().trim();
					if(isAssignableType(primitiveExpectedType, primitiveType, context))
					{
						return true;
					}
				}
			}

			return false;
		}
		if(expectedType == null || expectedType.equals("*") || expectedType.equals(OBJECT_CLASS_NAME) || expectedType.equals("Class"))
		{
			return true;
		}
		if(expectedType.equals(type))
		{
			return true;
		}

		final boolean nonecma = context.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4;

		if("Number".equals(expectedType) && ("int".equals(type) || "uint".equals(type) || (("Boolean".equals(type) || "String".equals(type)) && nonecma)))
		{
			return true;
		}
		if("int".equals(expectedType) && ("Number".equals(type) ||
				("String".equals(type) && nonecma) ||
				"uint".equals(type)))
		{
			return true; // compatibility
		}

		if("uint".equals(expectedType) && ("int".equals(type) || "Number".equals(type)))
		{
			return true;
		}
		if(ARRAY_TYPE_NAME.equals(type))
		{
			if(ARGUMENTS_TYPE_NAME.equals(expectedType) || expectedType.startsWith(ARRAY_TYPE_NAME) /*Array[*/ || JSTypeEvaluateManager.isArrayType(expectedType))
			{
				return true;
			}
		}

		if(ARGUMENTS_TYPE_NAME.equals(type) && nonecma)
		{
			return true; // TODO: hack for indirect call Array.slice(arguments, 0)
		}

		if(ARRAY_TYPE_NAME.equals(expectedType) && JSTypeEvaluateManager.isArrayType(type))
		{
			return true;
		}
		if("String".equals(expectedType) && (("Number".equals(type) || "int".equals(type) || "Boolean".equals(type) || "RegExp".equals(type)) && nonecma))
		{
			return true;
		}

		if("*".equals(type) || (OBJECT_CLASS_NAME.equals(type) && nonecma))
		{
			return true; // Dynamic cast
		}
		if("void".equals(type))
		{
			return false;
		}
		final PsiElement typeClass = type != null ? unwrapProxy(findClassByQName(type, context)) : null;

		if(!(typeClass instanceof JSClass))
		{
			return true;
		}

		boolean result = typeClass.processDeclarations(new ResolveProcessor(null)
		{
			{
				setToProcessHierarchy(true);
				setTypeContext(true);
				setToProcessMembers(false);
				setLocalResolve(true);
			}

			@Override
			public boolean execute(final PsiElement element, final ResolveState state)
			{
				if(!(element instanceof JSClass))
				{
					return true;
				}
				final JSClass clazz = (JSClass) element;
				boolean sameType = clazz.getQualifiedName().equals(expectedType);

				if(!sameType)
				{
					for(JSClass implementedClazz : clazz.getImplementedInterfaces())
					{
						sameType = !implementedClazz.processDeclarations(this, ResolveState.initial(), implementedClazz, implementedClazz);
						if(sameType)
						{
							break;
						}
					}
				}
				return !sameType;
			}
		}, ResolveState.initial(), typeClass, typeClass);

		if(result && isImplicitCastPossible((JSClass) typeClass, expectedType))
		{
			result = false;
		}

		return !result;
	}

	private static boolean hasMultipleOccurenceDelimiters(String expectedType)
	{
		final String commentDelimiters = JSType.COMMENT_DELIMITERS;

		for(int i = 0; i < commentDelimiters.length(); ++i)
		{
			if(expectedType.indexOf(commentDelimiters.charAt(i)) != -1)
			{
				return true;
			}
		}
		return false;
	}

	private static boolean isImplicitCastPossible(JSClass typeClass, String expectedType)
	{
		// TODO: move to flex support plugin
		if(expectedType.equals("flash.events.IEventDispatcher"))
		{
			JSAttributeList attributeList = typeClass.getAttributeList();
			if(attributeList != null)
			{
				return attributeList.getAttributesByName("Bindable").length != 0;
			}
		}
		return false;
	}

	public static PsiElement getTopReferenceParent(final PsiElement parent)
	{
		PsiElement currentParent = parent;
		for(; currentParent instanceof JSReferenceExpression; currentParent = currentParent.getParent())
		{
			;
		}
		return currentParent;
	}

	public static PsiElement getTopReferenceExpression(final @NotNull PsiElement parent)
	{
		PsiElement element = parent;

		for(PsiElement currentParent = parent.getParent(); currentParent instanceof JSReferenceExpression; element = currentParent, currentParent = currentParent.getParent())

		{
			;
		}
		return element;
	}

	public static boolean isSelfReference(final PsiElement currentParent, PsiElement elt)
	{
		return currentParent instanceof JSPackageStatement ||
				currentParent instanceof JSNamespaceDeclaration ||
				(currentParent instanceof JSVariable && (((JSVariable) currentParent).findNameIdentifier() == elt.getNode())) ||
				(currentParent instanceof JSFunction && ((JSFunction) currentParent).findNameIdentifier() == elt.getNode()) ||
				currentParent instanceof JSClass;
	}

	public static PsiElement findParent(final PsiElement element)
	{
		PsiElement parent = element instanceof JSVariable ? element.getParent().getParent() : element.getParent();
		return parent instanceof JSFile ? findParentClass((JSFile) parent) : parent;
	}

	private static PsiElement findParentClass(final JSFile file)
	{
		JSClass xmlBackedClass = getXmlBackedClass(file);
		if(xmlBackedClass != null)
		{
			return xmlBackedClass;
		}

		PsiElement forcedContext = file.getUserData(contextKey);
		if(forcedContext != null && !forcedContext.isValid())
		{
			forcedContext = null;
		}
		if(forcedContext instanceof JSClass)
		{
			return forcedContext;
		}

		if(forcedContext instanceof JSFile)
		{
			return findParentClass((JSFile) forcedContext);
		}

		if(forcedContext instanceof PsiFile && JavaScriptSupportLoader.isFlexMxmFile((PsiFile) forcedContext))
		{
			return XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) forcedContext);
		}

		if(forcedContext instanceof XmlElement)
		{
			PsiFile containingFile = forcedContext.getContainingFile();
			if(JavaScriptSupportLoader.isFlexMxmFile(containingFile))
			{
				return XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) containingFile);
			}
		}

		if(forcedContext != null)
		{
			PsiFile containingFile = forcedContext.getContainingFile();
			if(containingFile instanceof JSFile && containingFile != file)
			{
				return findParentClass((JSFile) containingFile);
			}

		}
		return file;
	}

	@Nullable
	public static JSClass getClassOfContext(PsiElement node)
	{
		final JSClass jsClass = PsiTreeUtil.getParentOfType(node, JSClass.class);
		if(jsClass != null)
		{
			return jsClass;
		}
		else
		{
			final PsiElement context = getClassReferenceForXmlFromContext(node.getContainingFile());
			if(context instanceof JSClass)
			{
				return (JSClass) context;
			}
		}

		return null;
	}

	public static
	@Nullable
	JSClass findClassOfQualifier(final JSExpression qualifier, final PsiFile containingFile)
	{
		final String s = getQualifiedExpressionType(qualifier, containingFile);
		final PsiElement qName = s != null ? unwrapProxy(findClassByQName(s, containingFile)) : null;
		if(qName instanceof JSClass)
		{
			return (JSClass) qName;
		}
		return null;
	}

	public static boolean referenceExpressionShouldBeQualified(final JSReferenceExpression contextExpr)
	{
		PsiElement parent = contextExpr.getParent();
		return parent instanceof JSImportStatement || (parent instanceof JSReferenceList && contextExpr.getContainingFile().getContext() != null);
	}

	public static boolean isArtificialClassUsedForReferenceList(JSClass clazz)
	{
		return clazz.getContainingFile().getContext() != null;
	}

	public static Collection<JSQualifiedNamedElement> findElementsByName(String name, Project project, GlobalSearchScope scope)
	{
		final Set<JSQualifiedNamedElement> result = new HashSet<JSQualifiedNamedElement>();
		Collection<JSQualifiedNamedElement> jsQualifiedNamedElements = StubIndex.getElements(JSNameIndex.KEY, name, project, scope, JSQualifiedNamedElement.class);

		for(JSQualifiedNamedElement e : jsQualifiedNamedElements)
		{
			result.add((JSQualifiedNamedElement) e.getNavigationElement());
		}

		Collection<VirtualFile> files = new ArrayList<VirtualFile>();
		files.addAll(FileBasedIndex.getInstance().getContainingFiles(FilenameIndex.NAME, name + JavaScriptSupportLoader.MXML_FILE_EXTENSION_DOT, scope));
		files.addAll(FileBasedIndex.getInstance().getContainingFiles(FilenameIndex.NAME, name + JavaScriptSupportLoader.MXML_FILE_EXTENSION2_DOT, scope));

		for(final VirtualFile file : files)
		{
			if(!file.isValid())
			{
				continue;
			}
			final PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
			if(psiFile != null)
			{
				result.add(XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) psiFile));
			}
		}
		return result;
	}

	public static boolean isNewResolveAndCompletion(PsiFile psiFile)
	{
		return psiFile.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4) || JavaScriptSupportLoader.isFlexMxmFile(psiFile);
	}

	public static String getTypeFromSetAccessor(final JSNamedElement jsNamedElement)
	{
		if(!(jsNamedElement instanceof JSFunction) || !((JSFunction) jsNamedElement).isSetProperty())
		{
			return null;
		}
		final JSParameterList jsParameterList = ((JSFunction) jsNamedElement).getParameterList();
		if(jsParameterList == null)
		{
			return null;
		}
		final JSParameter[] jsParameters = jsParameterList.getParameters();
		if(jsParameters == null || jsParameters.length != 1)
		{
			return null;
		}
		return jsParameters[0].getTypeString();
	}

	public static boolean processTopPackages(final ResolveProcessor processor, final ResolveState state, final Project project, final GlobalSearchScope scope)
	{
		return JSPackageIndex.processElementsInScope("", processor.getName(), new JSPackageIndex.PackageElementsProcessor()
		{
			@Override
			public boolean process(VirtualFile file, String name, JSPackageIndexInfo.Kind kind)
			{
				if(kind != JSPackageIndexInfo.Kind.PACKAGE)
				{
					return true;
				}
				return processor.execute(new JSPackageWrapper(name, project, scope), state);
			}
		}, scope, project);
	}

	public static boolean shouldProcessTopLevelGlobalContext(@NotNull PsiElement place, @NotNull PsiScopeProcessor processor)
	{
		PsiElement placeParent = null;

		return shouldProcessImports(place, processor) && (!((placeParent = place.getParent()) instanceof JSCallExpression) ||
				(place instanceof JSReferenceExpression &&
						((JSReferenceExpression) place).getQualifier() != null &&
						((ResolveProcessor) processor).specificallyAskingToResolveQualifiedNames()) ||
				placeParent instanceof JSNewExpression);
	}

	public static boolean shouldProcessImports(@NotNull PsiElement place, @NotNull PsiScopeProcessor processor)
	{
		if(!(processor instanceof ResolveProcessor) || ((ResolveProcessor) processor).isLocalResolve())
		{
			return false;
		}
		return (!(place instanceof JSReferenceExpression) ||
				((JSReferenceExpression) place).getQualifier() == null ||
				((ResolveProcessor) processor).specificallyAskingToResolveQualifiedNames());
	}

	public static boolean processTopLevelClasses(PsiScopeProcessor processor, ResolveState state, Project project, GlobalSearchScope scope, boolean acceptOnlyClasses, boolean acceptQualifiedElements)
	{
		boolean result = true;
		final String resolvedName = ((ResolveProcessor) processor).getName();

		if(resolvedName == null)
		{
			for(String s : StubIndex.getInstance().getAllKeys(JSNameIndex.KEY, project))
			{
				for(JSQualifiedNamedElement e : StubIndex.getElements(JSNameIndex.KEY, s, project, scope, JSQualifiedNamedElement.class))
				{
					if(ARGUMENTS_TYPE_NAME.equals(e.getName()))
					{
						continue;
					}
					if(acceptOnlyClasses && !(e instanceof JSClass))
					{
						continue;
					}

					if(!acceptQualifiedElements)
					{
						String qName = e.getQualifiedName();
						if(qName != null && qName.indexOf('.') != -1)
						{
							continue;
						}
					}
					result &= processor.execute(e, state);
				}
			}
		}
		else
		{
			for(JSQualifiedNamedElement e : StubIndex.getElements(JSQualifiedElementIndex.KEY, resolvedName, project, scope, JSQualifiedNamedElement.class))
			{
				if(!e.getName().equals(resolvedName))
				{
					continue;
				}
				if(acceptOnlyClasses && !(e instanceof JSClass))
				{
					continue;
				}
				result = processor.execute(e, state);
				if(!result)
				{
					break;
				}
			}
		}
		return result;
	}

	static boolean walkOverStructure(@NotNull PsiElement context, Processor<PsiNamedElement> processor)
	{
		PsiNamedElement parent = PsiTreeUtil.getNonStrictParentOfType(context, JSQualifiedNamedElement.class, PsiFile.class);

		if(parent instanceof JSClass)
		{
			PsiElement forcedContext = parent.getUserData(contextKey);

			if(forcedContext instanceof XmlBackedJSClassImpl)
			{
				if(!processor.process((PsiNamedElement) forcedContext))
				{
					return false;
				}
			}
		}

		while(parent != null)
		{
			if(!processor.process(parent))
			{
				return false;
			}

			if(parent instanceof JSPackageStatement)
			{
				break;
			}

			if(parent instanceof PsiFile)
			{
				final PsiElement data = parent.getUserData(contextKey);

				if(data instanceof JSElement)
				{
					parent = PsiTreeUtil.getNonStrictParentOfType(data, JSQualifiedNamedElement.class, PsiFile.class);
				}
				else
				{
					break;
				}
			}
			else
			{
				parent = PsiTreeUtil.getParentOfType(parent, JSQualifiedNamedElement.class, PsiFile.class);
			}
		}

		return true;
	}

	private static final Key<ParameterizedCachedValue<Set<String>, JSElement>> ourCachedOpenedNsesKey = Key.create("opened.nses");

	private static final UserDataCache<ParameterizedCachedValue<Set<String>, JSElement>, JSElement, Object> ourCachedOpenedNsesCache = new UserDataCache<ParameterizedCachedValue<Set<String>,
			JSElement>, JSElement, Object>()
	{
		@Override
		protected ParameterizedCachedValue<Set<String>, JSElement> compute(JSElement jsElement, Object p)
		{
			return CachedValuesManager.getManager(jsElement.getProject()).createParameterizedCachedValue(new ParameterizedCachedValueProvider<Set<String>, JSElement>()
			{
				@Override
				public CachedValueProvider.Result<Set<String>> compute(JSElement context)
				{
					class MyProcessor extends ResolveProcessor implements Processor<PsiNamedElement>
					{
						Set<String> openedNses;

						public MyProcessor()
						{
							super(null);
							putUserData(LOOKING_FOR_USE_NAMESPACES, true);
						}

						@Override
						public boolean process(PsiNamedElement psiNamedElement)
						{
							if(psiNamedElement instanceof JSElement)
							{
								processDeclarationsInScope((JSElement) psiNamedElement, this, ResolveState.initial(), psiNamedElement, psiNamedElement);
							}
							else
							{
								// TODO: XmlFile ?
							}
							return true;
						}

						@Override
						public boolean execute(PsiElement element, ResolveState state)
						{
							if(!(element instanceof JSUseNamespaceDirective))
							{
								return true;
							}
							if(openedNses == null)
							{
								openedNses = new THashSet<String>();
							}
							openedNses.add(((JSUseNamespaceDirective) element).getNamespaceToBeUsed());
							return true;
						}
					}
					MyProcessor processor = new MyProcessor();
					walkOverStructure(context, processor);
					return new CachedValueProvider.Result<Set<String>>(processor.openedNses, PsiModificationTracker.EVER_CHANGED);
				}
			}, false);
		}
	};

	public static Set<String> calculateOpenNses(PsiElement place)
	{
		final Ref<Set<String>> result = new Ref<Set<String>>();
		walkOverStructure(place, new Processor<PsiNamedElement>()
		{
			@Override
			public boolean process(PsiNamedElement psiNamedElement)
			{
				if(psiNamedElement instanceof JSElement)
				{
					result.set(ourCachedOpenedNsesCache.get(ourCachedOpenedNsesKey, (JSElement) psiNamedElement, null).getValue((JSElement) psiNamedElement));
				}
				return false;
			}
		});
		return result.get() != null ? result.get() : Collections.<String>emptySet();
	}

	public static boolean processGlobalThings(PsiScopeProcessor processor, ResolveState state, PsiElement place, PsiElement context)
	{
		boolean result = true;
		final Project project = context.getProject();
		final GlobalSearchScope scope = getSearchScope(context);

		if(shouldProcessTopLevelGlobalContext(place, processor))
		{
			result = processTopPackages((ResolveProcessor) processor, state, project, scope);
		}

		if(result)
		{
			boolean acceptOnlyClasses = place instanceof JSReferenceExpression ? isExprInTypeContext((JSReferenceExpression) place) : false;

			result = processTopLevelClasses(processor, state, project, scope, acceptOnlyClasses, true);
		}
		return result;
	}

	public static
	@Nullable
	JSParameter findParameterForUsedArgument(@NotNull JSExpression mainOccurence, @NotNull JSArgumentList parent)
	{
		int paramIndex = 0;

		for(JSExpression expr : parent.getArguments())
		{
			if(expr == mainOccurence)
			{
				break;
			}
			paramIndex++;
		}

		final JSExpression methodExpr = ((JSCallExpression) parent.getParent()).getMethodExpression();
		if(methodExpr instanceof JSReferenceExpression)
		{
			final ResolveResult[] results = ((JSReferenceExpression) methodExpr).multiResolve(false);

			if(results.length > 0)
			{
				final PsiElement element = results[0].getElement();

				if(element instanceof JSFunction)
				{
					final JSParameterList parameterList = ((JSFunction) element).getParameterList();
					if(parameterList != null)
					{
						final JSParameter[] params = parameterList.getParameters();

						if(paramIndex < params.length)
						{
							return params[paramIndex];
						}
					}
				}
			}
		}
		return null;
	}

	public static abstract class JSInjectedFilesVisitor implements PsiLanguageInjectionHost.InjectedPsiVisitor, XmlBackedJSClassImpl.InjectedFileVisitor
	{
		@Override
		public void visit(@NotNull final PsiFile injectedPsi, @NotNull final List<PsiLanguageInjectionHost.Shred> places)
		{
			if(injectedPsi instanceof JSFile)
			{
				process((JSFile) injectedPsi);
			}
		}

		protected abstract void process(JSFile file);

		@Override
		public void visit(XmlTag rootTag, JSFile file)
		{
			process(file);
		}
	}

	@NonNls
	public static final String OBJECT_CLASS_NAME = "Object";

	private JSResolveUtil()
	{
	}

	private static final Key<CachedValue<PsiElement[]>> ourFileElementsValueKey = Key.create("file.elements");

	public static void treeWalkUp(PsiScopeProcessor processor, PsiElement elt, PsiElement lastParent, PsiElement place)
	{
		treeWalkUp(processor, elt, lastParent, place, null, null);
	}


	public static void treeWalkUp(PsiScopeProcessor processor, PsiElement elt, PsiElement lastParent, PsiElement place, PsiElement terminatingParent)
	{
		treeWalkUp(processor, elt, lastParent, place, terminatingParent, null);
	}

	private static void treeWalkUp(final PsiScopeProcessor processor, PsiElement elt, PsiElement lastParent, PsiElement place, PsiElement terminatingParent, PsiElement currentScope)
	{
		if(elt == null)
		{
			return;
		}

		PsiElement parentElement = elt.getContext();
		if(elt instanceof JSFunction || elt instanceof JSObjectLiteralExpression)
		{
			currentScope = elt;
		}

		if(parentElement instanceof JSDefinitionExpression || (parentElement instanceof JSAssignmentExpression && !(elt instanceof JSFunctionExpression)))
		{
			if(elt == terminatingParent)
			{
				return;
			}
			elt = parentElement.getParent();
			parentElement = elt.getParent(); // when walking a| = b, start from enclosing statement
			if(parentElement instanceof JSExpressionStatement)
			{
				elt = parentElement;
				parentElement = parentElement.getParent();
			}
			currentScope = elt;
		}
		else if(parentElement instanceof JSVariable && currentScope == null)
		{
			// when walking from variable init start from enclosing statement (function expr / object literal could reference that could reference
			// var in this case, better to check for any scope change)
			currentScope = parentElement;
			parentElement = parentElement.getParent();
		}

		final boolean parentIsClass = parentElement instanceof JSClass;
		int index = -1;
		PsiElement[] children = getChildren(parentElement);

		if(children != null && children.length > 0)
		{
			index = 0;
			for(PsiElement el : children)
			{
				if(el == elt)
				{
					break;
				}
				++index;
			}
		}

		boolean finish = false;
		PsiElement cur = elt;

		do
		{
			if(!cur.processDeclarations(processor, ResolveState.initial(), cur == elt ? lastParent : null, place))
			{
				if(processor instanceof ResolveProcessor)
				{
					return;
				}
			}
			if(terminatingParent == cur)
			{
				finish = true;
				break;
			}
			if(cur instanceof PsiFile || parentIsClass)
			{
				break;
			}
			if(cur instanceof JSStatement && parentElement instanceof JSIfStatement)
			{
				// Do not try to resolve variables from then branch in else branch.
				break;
			}

			if(index == -1)
			{
				cur = cur.getPrevSibling();
			}
			else
			{
				if(index != 0)
				{
					cur = children[--index];
				}
				else
				{
					cur = null;
				}
			}
		}
		while(cur != null);

		final PsiElement func = parentIsClass || finish ? null : processFunctionDeclarations(processor, parentElement);
		if(func != null)
		{
			return;
		}

		if(elt instanceof PsiFile)
		{
			if(elt instanceof XmlFile)
			{
				final XmlFile xmlFile = (XmlFile) elt;
				final XmlDocument document = xmlFile.getDocument();
				final XmlTag rootTag = document != null ? document.getRootTag() : null;
				final String rootNs = rootTag != null ? rootTag.getNamespace() : null;

				if(JavaScriptSupportLoader.isMxmlNs(rootNs))
				{
					processAllGlobals(processor, xmlFile, place);
				}
				else
				{
					if(rootTag != null && xmlFile.getFileType() == XmlFileType.INSTANCE)
					{ // TODO this is bindows specific
						processAllGlobals(processor, xmlFile, place);
					}
				}
			}
			else if(elt instanceof JSFile)
			{
				if(parentElement != null)
				{
					XmlTag tag = PsiTreeUtil.getParentOfType(parentElement, XmlTag.class);
					final PsiFile containingFile = parentElement.getContainingFile();

					while(tag != null)
					{
						if(XmlBackedJSClassImpl.isInlineComponentTag(tag))
						{
							for(JSVariable var : ourCachedPredefinedVars.get(ourCachedPredefinedVarsKey, (XmlFile) containingFile, null).getValue())
							{
								if(!processor.execute(var, ResolveState.initial()))
								{
									return;
								}
							}
							JSClass clazz = XmlBackedJSClassImpl.getXmlBackedClass(tag);
							clazz.processDeclarations(processor, ResolveState.initial(), clazz, place);
							return;
						}
						tag = PsiTreeUtil.getParentOfType(tag, XmlTag.class);
					}

					parentElement = containingFile;
				}
				else
				{
					parentElement = elt.getUserData(contextKey);
				}
			}
			if(!(elt instanceof JSExpressionCodeFragment) && parentElement == null)
			{
				return;
			}
		}

		if(finish)
		{
			return;
		}
		treeWalkUp(processor, parentElement, elt, place, terminatingParent, currentScope);
	}

	private static UserDataCache<CachedValue<List<JSVariable>>, XmlFile, Object> ourCachedPredefinedVars = new UserDataCache<CachedValue<List<JSVariable>>, XmlFile, Object>()
	{
		@Override
		protected CachedValue<List<JSVariable>> compute(final XmlFile xmlFile, Object p)
		{
			return CachedValuesManager.getManager(xmlFile.getProject()).createCachedValue(new CachedValueProvider<List<JSVariable>>()
			{
				@Override
				public Result<List<JSVariable>> compute()
				{
					SmartList<JSVariable> vars = new SmartList<JSVariable>();
					String qName = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile).getQualifiedName();
					vars.add(new ImplicitJSVariableImpl("outerDocument", qName, xmlFile));
					vars.add(new ImplicitJSVariableImpl("data", OBJECT_CLASS_NAME, xmlFile));
					return new Result<List<JSVariable>>(vars, xmlFile);
				}
			}, false);
		}
	};

	private static Key<CachedValue<List<JSVariable>>> ourCachedPredefinedVarsKey = Key.create("ourCachedPredefinedVarsKey");

	public static boolean processAllGlobals(final PsiScopeProcessor processor, final XmlFile xmlFile, final PsiElement place)
	{
		JSClass clazz = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
		if(!clazz.processDeclarations(processor, ResolveState.initial(), clazz, place))
		{
			return false;
		}
		return true;
	}

	private static PsiElement[] getChildren(final PsiElement element)
	{
		if(!(element instanceof JSFile) && (!(element instanceof JSBlockStatement) || !(element.getParent() instanceof JSNamedElement)))
		{
			return null;
		}
		CachedValue<PsiElement[]> value = element.getUserData(ourFileElementsValueKey);

		if(value == null)
		{
			value = CachedValuesManager.getManager(element.getProject()).createCachedValue(new CachedValueProvider<PsiElement[]>()
			{
				@Override
				public Result<PsiElement[]> compute()
				{
					return new Result<PsiElement[]>(element.getChildren(), element);
				}
			}, false);
			element.putUserData(ourFileElementsValueKey, value);
		}

		return value.getValue();
	}

	@Nullable
	private static PsiElement processFunctionDeclarations(final @NotNull PsiScopeProcessor processor, final @Nullable PsiElement context)
	{
		if(!(context instanceof JSElement))
		{
			return null;
		}
		PsiElement[] children = getChildren(context);

		if(context != null)
		{
			int index = children != null ? children.length - 1 : -1;
			PsiElement cur = index >= 0 ? children[index] : context.getLastChild();

			while(cur != null)
			{
				if(cur instanceof JSFunction || cur instanceof JSClass)
				{
					if(!processor.execute(cur, ResolveState.initial()))
					{
						if(processor instanceof ResolveProcessor)
						{
							return ((ResolveProcessor) processor).getResult();
						}
					}
				}

				if(index == -1)
				{
					cur = cur.getPrevSibling();
				}
				else
				{
					if(index != 0)
					{
						cur = children[--index];
					}
					else
					{
						cur = null;
					}
				}
			}
		}
		return null;
	}

	public static JSClass findDeclaringClass(final JSFunction method)
	{
		final Ref<JSClass> lastVisitedClass = new Ref<JSClass>();
		iterateOverridenMethodsUp(method, new Processor<JSClass>()
		{
			@Override
			public boolean process(JSClass jsClass)
			{
				lastVisitedClass.set(jsClass);
				return true;
			}
		}, true);


		Collection<JSClass> visited = new THashSet<JSClass>();
		visitAllImplementedInterfaces(lastVisitedClass.get(), visited, new Processor<JSClass>()
		{
			@Override
			public boolean process(JSClass jsClass)
			{
				// hierarchy may contain maximum one interface declaring a certain method
				JSFunction interfaceMethod = jsClass.findFunctionByNameAndKind(method.getName(), method.getKind());
				if(interfaceMethod != null)
				{
					// TODO check signature
					lastVisitedClass.set(jsClass);
					return false;
				}
				return true;
			}
		});

		return lastVisitedClass.get();
	}

	/**
	 * @return true if processor said enough
	 */
	private static boolean visitAllImplementedInterfaces(JSClass clazz, @NotNull Collection<JSClass> visited, Processor<JSClass> processor)
	{
		if(visited.contains(clazz))
		{
			return true;
		}
		if(clazz.isInterface())
		{
			visited.add(clazz);
			if(!processor.process(clazz))
			{
				return true;
			}
		}
		JSClass[] interfaces = clazz.isInterface() ? clazz.getSuperClasses() : clazz.getImplementedInterfaces();
		for(JSClass anInterface : interfaces)
		{
			if(visitAllImplementedInterfaces(anInterface, visited, processor))
			{
				return true;
			}
		}
		return false;
	}

	/**
	 * @return true if processor said enough
	 */
	private static boolean iterateOverridenMethodsUp(JSFunction function, Processor<JSClass> processor, boolean allowDirectParent)
	{
		PsiElement clazz = findParent(function);
		if(!(clazz instanceof JSClass))
		{
			return false;
		}
		final PsiElement directParent = clazz;

		while(true)
		{
			if((allowDirectParent || directParent != clazz) && !processor.process((JSClass) clazz))
			{
				return true;
			}

			JSAttributeList attributeList = function.getAttributeList();
			if(attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE))
			{
				return false;
			}

			// TODO check signature
			final JSNamedElement overridenMethod = findOverriddenMethod(function, (JSClass) clazz);
			if(!(overridenMethod instanceof JSFunction))
			{
				return false;
			}
			function = (JSFunction) overridenMethod;

			clazz = findParent(function);
			if(!(clazz instanceof JSClass))
			{
				return false;
			}
		}
	}

	public static boolean isReferenceTo(final PsiPolyVariantReference reference, final String referencedName, PsiElement _element)
	{
		String elementName;
		if(_element instanceof JSNamedElement)
		{
			elementName = ((PsiNamedElement) _element).getName();
		}
		else if(_element instanceof XmlAttributeValue)
		{
			elementName = ((XmlAttributeValue) _element).getValue();
		}
		else if(_element instanceof PsiFile)
		{
			elementName = ((PsiFile) _element).getVirtualFile().getNameWithoutExtension();
		}
		else if(_element instanceof PsiDirectoryContainer)
		{
			elementName = ((PsiNamedElement) _element).getName();
		}
		else
		{
			return false;
		}

		if(Comparing.equal(referencedName, elementName, true))
		{
			PsiElement element = _element;
			if(element instanceof JSNamedElementProxy)
			{
				element = ((JSNamedElementProxy) element).getElement();
			}
			final ResolveResult[] resolveResults = reference.multiResolve(true);

			for(ResolveResult r : resolveResults)
			{
				PsiElement resolvedElement = r.getElement();
				if(resolvedElement instanceof JSNamedElementProxy)
				{
					resolvedElement = ((JSNamedElementProxy) resolvedElement).getElement();
				}

				if(resolvedElement.isEquivalentTo(element) ||
						element.isEquivalentTo(resolvedElement) ||
						((element instanceof JSProperty || element instanceof XmlAttributeValue) &&
								resolvedElement != null &&
								resolvedElement.getParent() == element))
				{
					if(reference instanceof JSReferenceExpression && ((((JSReferenceExpression) reference).getParent() == resolvedElement && resolvedElement instanceof JSDefinitionExpression) ||
							resolvedElement instanceof JSFunctionExpression && ((JSFunctionExpression) resolvedElement).getFunction().findNameIdentifier().getTreeParent().getPsi() == reference))
					{
						return false; // do not include self to usages
					}

					return true;
				}

				if(resolvedElement instanceof JSFunctionExpression)
				{
					final ASTNode nameIdentifier = ((JSFunctionExpression) resolvedElement).getFunction().findNameIdentifier();
					if(nameIdentifier != null && nameIdentifier.getTreeParent().getPsi() == element)
					{
						return true;
					}
				}
				else if(resolvedElement instanceof JSFunction)
				{
					JSFunction fun = (JSFunction) resolvedElement;

					if(fun.isConstructor())
					{
						if((element instanceof JSClass && resolvedElement.getParent() == element) || (element instanceof JSFile &&
								reference.getElement().getParent() != resolvedElement &&
								isPublicEntityReferenceToJSFile(findParent(resolvedElement), element)))
						{
							return true;
						}
					}
					else if(element instanceof JSFunction)
					{
						JSFunction anotherFun = (JSFunction) element;
						boolean getProperty;

						if((getProperty = fun.isGetProperty()) || fun.isSetProperty())
						{
							if((getProperty && anotherFun.isSetProperty()) || (!getProperty && anotherFun.isGetProperty()))
							{
								PsiElement funParent = JSResolveUtil.findParent(fun);
								PsiElement parent = JSResolveUtil.findParent(anotherFun);
								if(funParent == parent || (parent != null && parent.isEquivalentTo(funParent)))
								{
									return true;
								}
							}
						}
						PsiElement resolvedElementParent = findParent(resolvedElement);
						final PsiElement elementParent = findParent(element);

						if(elementParent instanceof JSClass && resolvedElementParent instanceof JSClass)
						{
							final JSClass anotherClass = (JSClass) elementParent;

							final Collection<JSClass> visitedInterfaces = new THashSet<JSClass>();
							return iterateOverridenMethodsUp((JSFunction) resolvedElement, new Processor<JSClass>()
							{
								@Override
								public boolean process(JSClass jsClass)
								{
									if(anotherClass.isInterface())
									{
										return !visitAllImplementedInterfaces(jsClass, visitedInterfaces, new Processor<JSClass>()
										{
											@Override
											public boolean process(JSClass jsClass)
											{
												return !jsClass.isEquivalentTo(anotherClass);
											}
										});
									}
									else
									{
										return !jsClass.isEquivalentTo(anotherClass);
									}
								}
							}, anotherClass.isInterface());
						}
					}
				}
				else if(resolvedElement instanceof JSClass && element instanceof JSFunction && element.getParent() == resolvedElement)
				{
					return true;
				}

				if((resolvedElement instanceof JSClass ||
						resolvedElement instanceof JSNamespaceDeclaration ||
						resolvedElement instanceof JSFunction ||
						resolvedElement instanceof JSVariable) && ((element instanceof XmlFile && resolvedElement.getParent().getContainingFile() == element) || (element instanceof JSFile &&
						reference.getElement().getParent() != resolvedElement &&
						isPublicEntityReferenceToJSFile(resolvedElement, element))))
				{
					return true;
				}

				if(resolvedElement instanceof JSVariable && element instanceof JSFunction)
				{
					if(fieldIsImplicitAccessorMethod((JSFunction) element, (JSVariable) resolvedElement))
					{
						return true;
					}
				}
			}
		}
		return false;
	}

	private static boolean fieldIsImplicitAccessorMethod(JSFunction fun, JSVariable var)
	{
		if(!fun.isGetProperty() && !fun.isSetProperty())
		{
			return false;
		}
		PsiElement funParent = findParent(fun);
		if(!(funParent instanceof JSClass) || !((JSClass) funParent).isInterface())
		{
			return false;
		}
		JSAttributeList attributeList = var.getAttributeList();
		if(attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC)
		{
			return false;
		}

		PsiElement parent = findParent(var);
		if(!(parent instanceof JSClass))
		{
			return false;
		}

		for(JSClass c : ((JSClass) parent).getImplementedInterfaces())
		{
			if(c == funParent)
			{
				return true;
			}
		}

		return false;
	}

	private static boolean isPublicEntityReferenceToJSFile(PsiElement resolvedElement, PsiElement element)
	{
		PsiElement parent = resolvedElement.getParent();
		if(parent instanceof JSVarStatement)
		{
			parent = parent.getParent();
		}
		return parent instanceof JSPackageStatement && parent.getParent() == element;
	}

	private static JSNamedElement findOverriddenMethod(PsiElement resolvedElement, JSClass resolvedElementClass)
	{
		final Ref<JSNamedElement> override = new Ref<JSNamedElement>();

		iterateType((JSFunction) resolvedElement, resolvedElementClass, null, new OverrideHandler()
		{
			@Override
			public boolean process(ResolveProcessor processor, PsiElement scope, String className)
			{
				PsiElement result = processor.getResult();
				if(result instanceof JSNamedElement)
				{
					override.set((JSNamedElement) result);
					return false;
				}
				return true;
			}
		});
		return override.get();
	}

	public static boolean checkClassHasParentOfAnotherOne(final JSClass aClass, final JSClass parent, @Nullable Set<JSClass> visited)
	{
		if(visited != null && visited.contains(aClass))
		{
			return false;
		}

		for(JSClass superClazz : aClass.getSuperClasses())
		{
			if(superClazz == parent)
			{
				return true;
			}
			if(visited == null)
			{
				visited = new THashSet<JSClass>();
			}
			visited.add(aClass);
			return checkClassHasParentOfAnotherOne(superClazz, parent, visited);
		}
		return false;
	}

	private static final Key<CachedValue<Map<PsiPolyVariantReference, ResolveResult[]>>> MY_RESOLVED_CACHED_KEY = Key.create("JS.AllResolvedKey");
	private static UserDataCache<CachedValue<Map<PsiPolyVariantReference, ResolveResult[]>>, PsiFile, Object> ourCachedResolveCache = new UserDataCache<CachedValue<Map<PsiPolyVariantReference,
			ResolveResult[]>>, PsiFile, Object>()
	{

		@Override
		protected CachedValue<Map<PsiPolyVariantReference, ResolveResult[]>> compute(PsiFile file, Object o)
		{
			return CachedValuesManager.getManager(file.getProject()).createCachedValue(new CachedValueProvider<Map<PsiPolyVariantReference, ResolveResult[]>>()
			{
				@Override
				public Result<Map<PsiPolyVariantReference, ResolveResult[]>> compute()
				{
					return new Result<Map<PsiPolyVariantReference, ResolveResult[]>>(Collections.synchronizedMap(new HashMap<PsiPolyVariantReference, ResolveResult[]>()),
							PsiModificationTracker.MODIFICATION_COUNT);
				}
			}, false);
		}
	};

	public static
	@Nullable
	JSExpression getRealRefExprQualifier(final JSReferenceExpression expr)
	{
		final JSExpression qualifier = ((JSReferenceExpressionImpl) expr).getResolveQualifier();
		if(qualifier != null)
		{
			return qualifier;
		}

		if(isExprInTypeContext(expr))
		{
			final JSImportedElementResolveResult resolved = JSImportHandlingUtil.resolveTypeNameUsingImports(expr);
			if(resolved == null)
			{
				return expr.getQualifier();
			}

			return getRealRefExprQualifierFromResult(expr, resolved);
		}

		return qualifier;
	}

	public static JSExpression getRealRefExprQualifierFromResult(final JSReferenceExpression expr, final JSImportedElementResolveResult resolved)
	{
		return ((JSReferenceExpression) JSChangeUtil.createExpressionFromText(expr.getProject(), resolved.qualifiedName).getPsi()).getQualifier();
	}

	public static String findPackageStatementQualifier(final PsiElement context)
	{
		if(context instanceof JSClass)
		{
			final String s = ((JSClass) context).getQualifiedName();
			if(s != null)
			{
				final int i = s.lastIndexOf('.');
				if(i != -1)
				{
					return s.substring(0, i);
				}
				else
				{
					return null;
				}
			}
		}

		final JSPackageStatement packageStatement = PsiTreeUtil.getNonStrictParentOfType(context, JSPackageStatement.class);

		if(packageStatement != null)
		{
			return packageStatement.getQualifiedName();
		}

		return null;
	}

	public static boolean isExprInStrictTypeContext(final JSReferenceExpression expr)
	{
		final PsiElement parent = expr.getParent();
		boolean parentIsVar;

		return (((parentIsVar = (parent instanceof JSVariable)) || parent instanceof JSFunction) && parent.getNode().findChildByType(JSTokenTypes.COLON) != null &&
				(!parentIsVar || ((JSVariable) parent).getInitializer() != expr)) ||
				parent instanceof JSAttributeList ||
				parent instanceof JSGenericSignature ||
				parent instanceof JSImportStatement ||
				parent instanceof JSReferenceList;
	}

	public static boolean isExprInTypeContext(final JSReferenceExpression expr)
	{
		final PsiElement parent = expr.getParent();

		return isExprInStrictTypeContext(expr) ||
				parent instanceof JSNewExpression ||
				parent instanceof JSUseNamespaceDirective ||
				(parent instanceof JSBinaryExpression &&
						((JSBinaryExpression) parent).getROperand() == expr &&
						(((JSBinaryExpression) parent).getOperationSign() == JSTokenTypes.IS_KEYWORD || ((JSBinaryExpression) parent).getOperationSign() == JSTokenTypes.AS_KEYWORD));
	}

	public static PsiElement findClassByQName(final String link, final @NotNull PsiElement context)
	{
		final Module module = ModuleUtil.findModuleForPsiElement(context);
		return findClassByQName(link, JavaScriptIndex.getInstance(context.getProject()), module);
	}

	public static PsiElement findClassByQName(final String link, GlobalSearchScope scope, Project project)
	{
		return findClassByQName(link, JavaScriptIndex.getInstance(project), scope);
	}

	public static PsiElement findClassByQName(final String link, final JavaScriptIndex index, final Module module)
	{
		final GlobalSearchScope searchScope = getSearchScope(module, index.getProject());
		return findClassByQName(link, index, searchScope);
	}

	public static GlobalSearchScope getSearchScope(@NotNull PsiElement context)
	{
		return getSearchScope(ModuleUtil.findModuleForPsiElement(context), context.getProject());
	}

	public static GlobalSearchScope getSearchScope(@Nullable Module module, @NotNull Project project)
	{
		GlobalSearchScope allScope = (module != null ? module : project).getUserData(MY_SCOPE_KEY);
		if(allScope == null)
		{
			final GlobalSearchScope searchScope = module != null ? GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module) : GlobalSearchScope.allScope(project);
			allScope = new GlobalSearchScope()
			{
				@Override
				public boolean contains(VirtualFile file)
				{
					return searchScope.contains(file) || file.getUserData(IMPLICIT_JS_FILES_KEY) != null;
				}

				@Override
				public int compare(VirtualFile file1, VirtualFile file2)
				{
					return searchScope.compare(file1, file2);
				}

				@Override
				public boolean isSearchInModuleContent(@NotNull Module aModule)
				{
					return searchScope.isSearchInModuleContent(aModule);
				}

				@Override
				public boolean isSearchInLibraries()
				{
					return searchScope.isSearchInLibraries();
				}
			};
			(module != null ? module : project).putUserData(MY_SCOPE_KEY, allScope);
		}

		return allScope;
	}

	private static PsiElement findClassByQName(final String link, final JavaScriptIndex index, final GlobalSearchScope searchScope)
	{
		synchronized(index)
		{
			PsiElement element = index.recallClass(link, searchScope);
			if(element != null)
			{
				return element;
			}

			final PsiElement[] result = new PsiElement[1];

			final Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(JSQualifiedElementIndex.KEY, link, index.getProject(), searchScope, JSQualifiedNamedElement.class);
			for(JSQualifiedNamedElement clazz : candidates)
			{
				if(link.equals(clazz.getQualifiedName()))
				{
					if("Object".equals(link) && !JavaScriptIndex.ECMASCRIPT_JS2.equals(clazz.getContainingFile().getVirtualFile().getName()) // object from swf do
						// not contain necessary members!
							)
					{
						continue;
					}
					result[0] = clazz;
					break;
				}
			}

			if(result[0] == null)
			{
				String className = link.substring(link.lastIndexOf('.') + 1);
				if(className.length() > 0 &&
						(Character.isUpperCase(className.charAt(0)) || Character.isLowerCase(className.charAt(0))) &&
						!isBuiltInClassName(className))
				{
					// TODO optimization, remove when packages will be properly handled
					result[0] = findClassByQNameViaHelper(link, index, className, searchScope);
				}
			}
			final PsiElement psiElement = result[0];
			if(psiElement != null)
			{
				index.rememberTopLevelClassElement(link, searchScope, psiElement);
			}
			return psiElement;
		}
	}

	private static boolean isBuiltInClassName(final String className)
	{
		return OBJECT_CLASS_NAME.equals(className) || "Boolean".equals(className) || "Function".equals(className) || "String".equals(className);
	}

	public static boolean isPredefinedFile(PsiFile file)
	{
		return false;
	}

	@Nullable
	private static PsiElement findClassByQNameViaHelper(final String link, final JavaScriptIndex index, final String className, final GlobalSearchScope scope)
	{
		for(JSResolveHelper helper : Extensions.getExtensions(JSResolveHelper.EP_NAME))
		{
			PsiElement result = helper.findClassByQName(link, index, className, scope);
			if(result != null)
			{
				return result;
			}
		}
		return null;
	}

	public static JSPackage findPackageByText(final String str, final JavaScriptIndex index)
	{
		final PsiElement element = doFindPackage(str, JavaScriptIndex.getInstance(index.getProject()), null);
		if(element instanceof MyPackageWrapper)
		{
			return ((MyPackageWrapper) element).myPackage;
		}
		return null;
	}

	private static PsiElement doFindPackage(final String qName, final JavaScriptIndex index, @Nullable Module module)
	{
		JSPackage jsPackage;

		synchronized(index)
		{
			final PsiElement element = index.recallPackageElement(qName);
			if(element != null)
			{
				return element;
			}

			jsPackage = index.getDefaultPackage();
			StringTokenizer tokenizer = new StringTokenizer(qName, ".");

			while(tokenizer.hasMoreElements())
			{
				String nextElement = tokenizer.nextElement();
				if(nextElement != null)
				{
					jsPackage = jsPackage.findPackageWithNameId(index.getIndexOf(nextElement));
				}

				if(jsPackage == null || nextElement == null)
				{
					jsPackage = null;
					break;
				}
			}

			final PsiElement myWrapper = jsPackage != null ? new MyPackageWrapper(jsPackage, index.getProject()) : null;
			if(myWrapper != null)
			{
				index.rememberPackageElement(qName, myWrapper);
			}
			return myWrapper;
		}
	}

	public static class MyPackageWrapper extends PsiElementBase implements JSNamedElement
	{
		final JSPackage myPackage;
		final private Project project;

		MyPackageWrapper(JSPackage _package, Project _project)
		{
			myPackage = _package;
			project = _project;
		}

		@Override
		public String getName()
		{
			return JavaScriptIndex.getInstance(project).getStringByIndex(myPackage.getNameId());
		}

		@Override
		public PsiElement setName(@NonNls @NotNull final String name) throws IncorrectOperationException
		{
			throw new IncorrectOperationException();
		}

		@Override
		public PsiElement getNameIdentifier()
		{
			return null;
		}

		@Override
		@NotNull
		public Language getLanguage()
		{
			return JavaScriptSupportLoader.JAVASCRIPT.getLanguage();
		}

		@Override
		@NotNull
		public PsiElement[] getChildren()
		{
			return PsiElement.EMPTY_ARRAY;
		}

		@Override
		public PsiElement getParent()
		{
			return null;
		}

		@Override
		public PsiFile getContainingFile()
		{
			return null;
		}

		@Override
		public PsiElement getFirstChild()
		{
			return null;
		}

		@Override
		public boolean isValid()
		{
			return true;
		}

		@Override
		public PsiElement getLastChild()
		{
			return null;
		}

		@Override
		public PsiElement getNextSibling()
		{
			return null;
		}

		@Override
		public PsiElement getPrevSibling()
		{
			return null;
		}

		@Override
		public TextRange getTextRange()
		{
			return null;
		}

		@Override
		public int getStartOffsetInParent()
		{
			return 0;
		}

		@Override
		@NotNull
		public Project getProject()
		{
			return project;
		}

		@Override
		public int getTextLength()
		{
			return 0;
		}

		@Override
		public PsiElement findElementAt(final int offset)
		{
			return null;
		}

		@Override
		public int getTextOffset()
		{
			return 0;
		}

		@Override
		public String getText()
		{
			return null;
		}

		@Override
		@NotNull
		public char[] textToCharArray()
		{
			return new char[0];
		}

		@Override
		public boolean textContains(final char c)
		{
			return false;
		}

		@Override
		public ASTNode getNode()
		{
			return null;
		}

		@Override
		public ASTNode findNameIdentifier()
		{
			return null;
		}

		public JSPackage getPackage()
		{
			return myPackage;
		}
	}

	public static int[] buildNameIdsForQualifier(final JSExpression qualifier, final JavaScriptIndex index)
	{
		int[] nameIds = null;

		if(qualifier == null)
		{
			nameIds = JSSymbolUtil.buildNameIndexArray(qualifier, null, index);
		}
		else
		{
			ContextResolver resolver = new ContextResolver(qualifier);
			nameIds = resolver.getQualifierAsNameIndex(index);

			if(nameIds == null)
			{
				nameIds = new int[]{index.getIndexOf("")};
			}
		}

		return nameIds;
	}

	public static JSExpression findClassIdentifier(JSExpression _qualifier)
	{
		JSExpression qualifier = _qualifier;
		if(qualifier instanceof JSReferenceExpression && PROTOTYPE_FIELD_NAME.equals(((JSReferenceExpression) qualifier).getReferencedName()))
		{
			qualifier = ((JSReferenceExpression) qualifier).getQualifier();
			if(qualifier == null)
			{
				qualifier = _qualifier;
			}
		}

		if(qualifier instanceof JSReferenceExpression && ((JSReferenceExpression) qualifier).getQualifier() == null)
		{
			qualifier = JSSymbolUtil.findReferenceExpressionUsedForClassExtending((JSReferenceExpression) qualifier);
		}
		return qualifier;
	}

	public static boolean iterateType(final JSFunction node, final PsiElement jsClass, final String typeName, final OverrideHandler handler)
	{
		final Project project = jsClass.getProject();

		if(jsClass instanceof JSClass)
		{
			String namespace = null;
			JSAttributeList attributeList = node.getAttributeList();
			if(attributeList != null)
			{
				namespace = attributeList.getNamespace();
			}
			return processOverrides(jsClass, handler, node.getName(), namespace, node);
		}

		return JSTypeEvaluateManager.getInstance(project).iterateTypeHierarchy(typeName, new JSTypeEvaluateManager.NamespaceProcessor()
		{
			final JavaScriptIndex index = JavaScriptIndex.getInstance(project);
			final Set<JSNamespace> visited = new THashSet<JSNamespace>();

			@Override
			public boolean process(final JSNamespace jsNamespace)
			{
				if(visited.contains(jsNamespace))
				{
					return true;
				}
				visited.add(jsNamespace);
				String className = null;

				final ResolveProcessor processor = new ResolveProcessor(node.getName(), node);

				if(jsClass instanceof JSClass)
				{
					final String baseQName = jsNamespace.getQualifiedName(index);
					final PsiElement clazzProxy = findClassByQName(baseQName, jsClass);

					if(clazzProxy != null)
					{
						JSNamedElement clazz = (JSNamedElement) unwrapProxy(clazzProxy);
						className = clazz.getName();

						if(clazz instanceof JSClass)
						{
							className = ((JSClass) clazz).getQualifiedName();
							clazz.processDeclarations(processor, ResolveState.initial(), clazz, node);
						}
					}
					else
					{
						return false;
					}
				}
				else if(jsNamespace.getPackage() != null)
				{
					className = jsNamespace.getQualifiedName(index);

					jsNamespace.processDeclarations(new JavaScriptSymbolProcessor.DefaultSymbolProcessor()
					{
						@Override
						protected boolean process(final PsiElement namedElement, final JSNamespace namespace)
						{
							return processor.execute(namedElement, ResolveState.initial());
						}

						@Override
						public PsiFile getBaseFile()
						{
							return null;
						}

						@Override
						public int getRequiredNameId()
						{
							return index.getIndexOf(node.getName());
						}
					});
				}

				if(processor.getResult() != null)
				{
					final PsiElement result = processor.getResult();
					if(result instanceof JSFunction)
					{
						final JSAttributeList attributeList = ((JSFunction) result).getAttributeList();
						if(attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE)
						{
							return false;
						}
					}
					handler.process(processor, jsClass, className);
					return false;
				}
				return true;
			}
		});
	}

	//public static Object ANY_NAMESPACE_MARKER = new Object();

	public static boolean processOverrides(final PsiElement jsClass, final OverrideHandler handler, String name, final Object namespace, final PsiElement context)
	{
		JSClass clazz = (JSClass) jsClass;
		final ResolveProcessor resolveProcessor = new ResolveProcessor(name, context)
		{
			{
				setToProcessHierarchy(true);
			}

			@Override
			public boolean execute(final PsiElement element, final ResolveState state)
			{
				if(!(element instanceof JSFunction))
				{
					return true;
				}
				JSFunction fun = (JSFunction) element;
				final JSAttributeList attributeList = fun.getAttributeList();
				if(attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE)
				{
					return false;
				}

				if((namespace == null && attributeList != null && attributeList.getNamespace() != null) || (namespace != null && (attributeList == null || !namespace.equals(attributeList
						.getNamespace()))))
				{
					return true;
				}
				return super.execute(element, state);
			}
		};

		for(JSClass superClazz : clazz.getSuperClasses())
		{
			if(superClazz == clazz)
			{
				break;
			}
			final boolean b = superClazz.processDeclarations(resolveProcessor, ResolveState.initial(), superClazz, context);

			if(!b)
			{
				final PsiElement element = resolveProcessor.getResult();
				if(element == null)
				{
					continue;
				}
				PsiElement parent = findParent(element);

				if(parent instanceof JSClass && !handler.process(resolveProcessor, superClazz, ((JSClass) parent).getQualifiedName()))
				{
					return false;
				}
			}
		}

		return true;
	}

	public static String getQNameToStartHierarchySearch(final JSFunction node)
	{
		PsiElement parentNode = node.getParent();
		parentNode = getClassReferenceForXmlFromContext(parentNode);

		if(parentNode instanceof JSClass)
		{
			final JSAttributeList attributeList = node.getAttributeList();

			if(attributeList == null ||
					!attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE) ||
					attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) ||
					attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE)
			{
				return null;
			}

			if(parentNode instanceof JSClass)
			{
				return ((JSClass) parentNode).getQualifiedName();
			}
		}
		else if(node instanceof JSFunctionExpression && parentNode.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4)
		{
			final ContextResolver resolver = new ContextResolver(node.getFirstChild());
			return resolver.getQualifierAsString();
		}
		else if(parentNode instanceof JSFile && parentNode.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4)
		{
			return node.getName();
		}
		return null;
	}

	public static boolean isInPlaceWhereTypeCanBeDuringCompletion(PsiElement expr)
	{
		final PsiElement parent = expr.getParent();
		if(parent instanceof JSArgumentList || (parent instanceof JSVariable && ((JSVariable) parent).getInitializer() == expr))
		{
			return true;
		}
		if(parent instanceof JSExpressionStatement)
		{
			if(expr instanceof JSReferenceExpression)
			{
				return ((JSReferenceExpression) expr).getQualifier() == null;
			}

			return true;
		}
		return false;
	}

	public static boolean isPlaceWhereNsCanBe(final PsiElement parent)
	{
		final PsiElement grandParent = parent.getParent();
		return grandParent instanceof JSClass || grandParent instanceof JSPackageStatement || (grandParent instanceof JSFile && grandParent.getContext() == null);
	}

	public static
	@Nullable
	String getTypeFromTagNameInMxml(final @Nullable PsiElement psiElement)
	{
		JSClass clazz = getClassFromTagNameInMxml(psiElement);
		return clazz != null ? clazz.getQualifiedName() : null;
	}

	public static JSClass getClassFromTagNameInMxml(final PsiElement psiElement)
	{
		XmlTag tag = psiElement != null ? PsiTreeUtil.getNonStrictParentOfType(psiElement, XmlTag.class) : null;
		if(tag != null && (tag.getNamespacePrefix().length() > 0 || JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile())))
		{
			if(isScriptContextTag(tag))
			{
				tag = ((XmlFile) tag.getContainingFile()).getDocument().getRootTag();
			}
			final XmlElementDescriptor descriptor = tag.getDescriptor();

			if(descriptor != null)
			{
				PsiElement decl = descriptor.getDeclaration();
				if(decl instanceof JSNamedElementProxy)
				{
					decl = ((JSNamedElementProxy) decl).getElement();
				}
				if(decl instanceof JSClass)
				{
					return ((JSClass) decl);
				}
				else if(decl instanceof XmlFile)
				{
					return XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) decl);
				}
			}
		}
		return null;
	}

	private static boolean isScriptContextTag(final XmlTag tag)
	{
		final String localName = tag.getLocalName();
		return "Script".equals(localName) || (localName.length() > 0 && Character.isLowerCase(localName.charAt(0)) && !"method".equals(localName));
	}

	public static boolean processMetaAttributesForClass(final PsiElement jsClass, final MetaDataProcessor processor)
	{
		return doProcessMetaAttributesForClass(unwrapProxy(jsClass), processor, null, true);
	}

	private static boolean doProcessMetaAttributesForClass(final PsiElement jsClass, final MetaDataProcessor processor, PsiElement lastParent, boolean forward)
	{
		if(jsClass instanceof JSClass)
		{
			if(OBJECT_CLASS_NAME.equals((((JSClass) jsClass).getQualifiedName())))
			{
				return true;
			}
			final PsiElement[] elements = getStubbedChildren(jsClass.getParent());
			int ind = elements.length - 1;
			while(ind >= 0 && elements[ind] != jsClass)
			{
				--ind;
			}
			--ind;


			while(ind >= 0)
			{
				final PsiElement current = elements[ind];

				if(current instanceof JSIncludeDirective)
				{
					if(!processIncludeDirective(processor, jsClass, (JSIncludeDirective) current, false))
					{
						return false;
					}
					--ind;
				}
				else
				{
					break;
				}
			}

			final JSAttributeList attributeList = ((JSClass) jsClass).getAttributeList();
			if(attributeList != null)
			{
				if(!processAttributeList(processor, jsClass, attributeList, true))
				{
					return false;
				}
			}
		}

		final PsiElement[] elements = getStubbedChildren(jsClass);
		Ref<PsiElement> continuePassElement = new Ref<PsiElement>();

		for(int i = forward ? 0 : elements.length - 1; i < elements.length && i >= 0; i += (forward ? 1 : -1))
		{
			PsiElement el = elements[i];

			if(el instanceof JSIncludeDirective)
			{
				if(!processIncludeDirective(processor, lastParent, (JSIncludeDirective) el, forward))
				{
					return false;
				}
			}
			else if(el instanceof JSAttributeList /*&& lastParent != null*/)
			{
				if(!processAttributeList(processor, lastParent, (JSAttributeList) el, forward))
				{
					return false;
				}
			}
			else
			{
				continuePassElement.set(null);
				if(!processor.handleOtherElement(el, jsClass, continuePassElement))
				{
					return false;
				}
				PsiElement nextEl = continuePassElement.get();

				if(nextEl instanceof JSAttributeListOwner)
				{
					JSAttributeList attributeList = ((JSAttributeListOwner) nextEl).getAttributeList();

					if(attributeList != null)
					{
						if(!processAttributeList(processor, nextEl, attributeList, forward))
						{
							return false;
						}
					}
				}

			}
		}
		return true;
	}

	public static boolean processAttributeList(final MetaDataProcessor processor, final PsiElement el, final JSAttributeList attributeList, boolean forward)
	{
		final PsiElement[] elements = getStubbedChildren(attributeList);
		for(int i = forward ? 0 : elements.length - 1; i < elements.length && i >= 0; i += (forward ? 1 : -1))
		{
			final PsiElement cur = elements[i];

			if(cur instanceof JSIncludeDirective)
			{
				if(!processIncludeDirective(processor, el, (JSIncludeDirective) cur, forward))
				{
					return false;
				}
			}
			else if(cur instanceof JSAttribute)
			{
				if(!processor.process((JSAttribute) cur))
				{
					return false;
				}
			}
			else if(cur instanceof JSNamedElement)
			{
				break;
			}
		}

		if(!processor.handleOtherElement(attributeList, el, null))
		{
			return false;
		}

		return true;
	}

	private static boolean processIncludeDirective(final MetaDataProcessor processor, final PsiElement lastParent, final JSIncludeDirective el, boolean forward)
	{
		final PsiFile file = el.resolveFile();

		if(file instanceof JSFile)
		{
			if(!doProcessMetaAttributesForClass(file, processor, lastParent, forward))
			{
				return false;
			}
		}

		return true;
	}

	public static PsiElement unwrapProxy(PsiElement jsClass)
	{
		if(jsClass instanceof JSNamedElementProxy)
		{
			jsClass = ((JSNamedElementProxy) jsClass).getElement();
		}
		return jsClass;
	}

	public static interface Resolver<T extends PsiPolyVariantReference>
	{
		ResolveResult[] doResolve(T t, PsiFile contextFile);
	}

	public static <T extends PsiPolyVariantReference> ResolveResult[] resolve(final PsiFile file, T instance, Resolver<T> resolver)
	{
		if(file == null)
		{
			return ResolveResult.EMPTY_ARRAY;
		}

		final Map<PsiPolyVariantReference, ResolveResult[]> resultsMap = ourCachedResolveCache.get(MY_RESOLVED_CACHED_KEY, file, null).getValue();
		ResolveResult[] results = resultsMap.get(instance);
		if(results != null)
		{
			return results;
		}

		results = resolver.doResolve(instance, file);
		resultsMap.put(instance, results);

		return results;
	}

	public static void clearResolveCaches(final PsiFile file)
	{
		file.putUserData(MY_RESOLVED_CACHED_KEY, null);
	}

	public static JSDefinitionExpression getDefinitionExpr(JSExpressionStatement exprStatement)
	{
		final JSExpression expression = exprStatement.getExpression();

		if(expression instanceof JSAssignmentExpression)
		{
			return (JSDefinitionExpression) ((JSAssignmentExpression) expression).getLOperand();
		}
		return null;
	}

	public static boolean processDeclarationsInScope(final JSElement _scope, final PsiScopeProcessor processor, final ResolveState state, final PsiElement lastParent, final PsiElement place)
	{
		JSElement scope = PsiUtilBase.getOriginalElement(_scope, _scope.getClass());
		if(scope == null)
		{
			return true;
		}
		String requiredName = null;

		if(processor instanceof ResolveProcessor)
		{
			requiredName = ((ResolveProcessor) processor).getName();
		}

		boolean result = true;
		final TIntObjectHashMap<Object> defsMap = ourCachedDefsCache.get(MY_CACHED_STATEMENTS, scope, null).getValue();

		if(requiredName == null)
		{
			TIntObjectIterator<Object> iterator = defsMap.iterator();
			while(iterator.hasNext())
			{
				iterator.advance();
				result = dispatchResolve(processor, state, place, result, iterator.value());
			}
		}
		else
		{
			final Object defs = defsMap.get(requiredName.hashCode());
			if(defs != null)
			{
				result = dispatchResolve(processor, state, place, result, defs);
			}
		}

		return result;
	}

	private static boolean dispatchResolve(final PsiScopeProcessor processor, final ResolveState state, final PsiElement place, boolean result, final Object o)
	{
		if(o instanceof JSElement[])
		{
			for(JSElement s : (JSElement[]) o)
			{
				result &= s.processDeclarations(processor, state, null, place);
			}
		}
		else
		{
			JSElement s = (JSElement) o;
			result &= s.processDeclarations(processor, state, null, place);
		}
		return result;
	}

	public static PsiElement getLocalVariableRef(JSFunction function, final JSReferenceExpression expr)
	{
		final ResolveProcessor processor = new ResolveProcessor(expr.getReferencedName(), true);

		while(function != null)
		{
			final boolean val = function.processDeclarations(processor, ResolveState.initial(), function.getParameterList(), function);
			if(!val)
			{
				return processor.getResult();
			}
			function = PsiTreeUtil.getParentOfType(function, JSFunction.class);
			if(function == null)
			{
				break;
			}
		}
		return null;
	}

	public static class MyResolveResult implements ResolveResult
	{
		private final PsiElement myFunction;
		private final JSImportStatement myImportUsed;
		private boolean myValidResult;

		public MyResolveResult(final PsiElement function)
		{
			this(function, true);
		}

		public MyResolveResult(final PsiElement function, JSImportStatement importUsed)
		{
			this(function, importUsed, true);
		}

		public MyResolveResult(final PsiElement function, boolean validResult)
		{
			this(function, null, validResult);
		}

		public MyResolveResult(final PsiElement function, JSImportStatement importUsed, boolean validResult)
		{
			myFunction = function;
			myValidResult = validResult;
			myImportUsed = importUsed;
		}

		@Override
		public PsiElement getElement()
		{
			return myFunction;
		}

		@Override
		public boolean isValidResult()
		{
			return myValidResult;
		}

		public void setValid(final boolean b)
		{
			myValidResult = b;
		}

		public JSImportStatement getImportUsed()
		{
			return myImportUsed;
		}
	}

	public static Key<PsiElement> contextKey = Key.create("context.key"); // JSElement or XmlElement

	public static class RelevantDefsUserDataCache extends UserDataCache<CachedValue<TIntObjectHashMap<Object>>, JSElement, Object>
	{

		@Override
		protected CachedValue<TIntObjectHashMap<Object>> compute(final JSElement jsElement, final Object o)
		{
			return CachedValuesManager.getManager(jsElement.getProject()).createCachedValue(new CachedValueProvider<TIntObjectHashMap<Object>>()
			{
				@Override
				public Result<TIntObjectHashMap<Object>> compute()
				{
					final TIntObjectHashMap<Object> relevantDefs = new TIntObjectHashMap<Object>();
					final MyJSElementVisitor elementVisitor = new MyJSElementVisitor(jsElement, relevantDefs);
					elementVisitor.startVisiting(jsElement);

					return new Result<TIntObjectHashMap<Object>>(relevantDefs, jsElement);
				}
			}, false);
		}

		private static class MyJSElementVisitor extends JSElementVisitor
		{
			private HashMap<String, Boolean> checkedVarsToOurStatus;
			private Set<JSFile> visitedIncludes;
			private final TIntObjectHashMap<Object> myRelevantDefs;
			private final JSElement myBase;
			private JSElement context;

			public MyJSElementVisitor(JSElement base, final TIntObjectHashMap<Object> relevantDefs)
			{
				myRelevantDefs = relevantDefs;
				myBase = base;
				context = base;
			}

			private void startVisiting(JSElement jsElement)
			{
				PsiElement first = null;

				JSSourceElement[] arrayElementsToScan = null;

				if(jsElement instanceof JSFunction)
				{
					final JSSourceElement[] body = ((JSFunction) jsElement).getBody();
					if(body.length > 0 && body[0] instanceof JSBlockStatement)
					{
						final ASTNode node = body[0].getNode().findChildByType(JSElementTypes.STATEMENTS);
						first = node != null ? node.getPsi() : null;
					}
				}
				else if(jsElement instanceof JSFile)
				{
					if(myBase == context)
					{
						first = jsElement.getFirstChild();
					}
					else
					{
						arrayElementsToScan = getSourceElements(jsElement);
					}
				}
				else if(jsElement instanceof JSClass || jsElement instanceof JSPackageStatement)
				{
					arrayElementsToScan = getSourceElements(jsElement);
				}
				else
				{
					first = jsElement.getFirstChild();
				}

				if(arrayElementsToScan != null)
				{
					for(JSSourceElement elt : arrayElementsToScan)
					{
						elt.accept(this);
					}
				}
				else
				{
					for(PsiElement e = first; e != null; e = e.getNextSibling())
					{
						if(e instanceof JSSourceElement)
						{
							e.accept(this);
						}
					}
				}
			}

			@Override
			public void visitJSDefinitionExpression(final JSDefinitionExpression node)
			{
				final JSExpression definedExpr = node.getExpression();
				if(definedExpr instanceof JSReferenceExpression && ((JSReferenceExpression) definedExpr).getQualifier() == null && false)
				{
					final String s = definedExpr.getText();
					if(checkedVarsToOurStatus == null)
					{
						checkedVarsToOurStatus = new HashMap<String, Boolean>(3);
					}
					Boolean aBoolean = checkedVarsToOurStatus.get(s);

					if(aBoolean == null)
					{
						final boolean isInjectedFile = isInjectedFile(myBase);
						PsiElement element = null;

						if(myBase instanceof JSFunction || isInjectedFile)
						{
							final ResolveProcessor processor = new ResolveProcessor(s);
							final PsiElement baseParent = myBase.getParent();
							treeWalkUp(processor, baseParent, baseParent, baseParent);
							element = processor.getResult();
						}

						aBoolean = (element == null) ? Boolean.TRUE : Boolean.FALSE;
						checkedVarsToOurStatus.put(s, aBoolean);
					}

					if(aBoolean == Boolean.TRUE)
					{
						addRelevantDef(node);
					}
				}
			}

			@Override
			public void visitJSVariable(JSVariable node)
			{
				addRelevantDef(node);
			}

			@Override
			public void visitJSIncludeDirective(final JSIncludeDirective includeDirective)
			{
				final PsiFile _file = includeDirective.resolveFile();

				if(!(_file instanceof JSFile) || myBase.getContainingFile() == _file)
				{
					return;
				}
				final JSFile file = (JSFile) _file;
				if(visitedIncludes != null && visitedIncludes.contains(file))
				{
					return;
				}
				if(visitedIncludes == null)
				{
					visitedIncludes = new THashSet<JSFile>();
				}
				visitedIncludes.add(file);

				final JSElement prevContext = context;
				context = file;
				context.putUserData(contextKey, myBase);
				startVisiting(file);
				context = prevContext;
			}

			@Override
			public void visitJSVarStatement(final JSVarStatement node)
			{
				for(JSVariable var : node.getVariables())
				{
					if(!var.isLocal())
					{
						var.accept(this);
					}
				}
			}

			@Override
			public void visitJSParameter(JSParameter node)
			{
			}

			@Override
			public void visitJSFunctionExpression(final JSFunctionExpression node)
			{
				// do not go inside other funcs
			}

			@Override
			public void visitJSObjectLiteralExpression(final JSObjectLiteralExpression node)
			{
				// do not go inside other funcs
			}

			@Override
			public void visitJSClass(final JSClass aClass)
			{
				// do not go inside other funcs
			}

			@Override
			public void visitJSPackageStatement(JSPackageStatement packageStatement)
			{
				// do not go inside other funcs
			}

			@Override
			public void visitJSFunctionDeclaration(final JSFunction node)
			{
				addRelevantDef(node);
			}

			@Override
			public void visitJSImportStatement(final JSImportStatement importStatement)
			{
				// do not expand tree
			}

			@Override
			public void visitJSUseNamespaceDirective(final JSUseNamespaceDirective useNamespaceDirective)
			{
				final String namespaceToBeUsed = useNamespaceDirective.getNamespaceToBeUsed();
				if(namespaceToBeUsed != null)
				{
					addNamedElement(useNamespaceDirective, namespaceToBeUsed.hashCode());
				}
			}

			private void addRelevantDef(final JSNamedElement node)
			{
				final String name = node.getName();

				if(name != null)
				{
					final int key = name.hashCode();
					addNamedElement(node, key);
				}
			}

			private void addNamedElement(final JSElement node, final int key)
			{
				final Object o = myRelevantDefs.get(key);

				if(o == null)
				{
					myRelevantDefs.put(key, node);
				}
				else if(o instanceof JSElement)
				{
					final JSElement[] newO = new JSElement[]{
							(JSElement) o,
							node
					};
					myRelevantDefs.put(key, newO);
				}
				else
				{
					final JSElement[] oldO = (JSElement[]) o;
					final JSElement[] newO = new JSElement[oldO.length + 1];
					System.arraycopy(oldO, 0, newO, 0, oldO.length);
					newO[oldO.length] = node;
					myRelevantDefs.put(key, newO);
				}
			}

			@Override
			public void visitElement(final PsiElement element)
			{
				element.acceptChildren(this);
			}
		}
	}

	public static class ContextResolver
	{
		private JSElement qualifyingExpression;
		private JSNamedElement parentContainer;
		private String typeName;

		public ContextResolver(JSExpression expr)
		{
			if(expr instanceof JSThisExpression || expr instanceof JSSuperExpression)
			{
				// We need to resolve ns mapping for 'this', which function was the constructor of the object
				resolveContext(expr);
			}
			else
			{
				qualifyingExpression = expr instanceof JSReferenceExpression ? JSSymbolUtil.findReferenceExpressionUsedForClassExtending((JSReferenceExpression) expr) : expr;
			}
		}

		public ContextResolver(PsiElement element)
		{
			resolveContext(element);
		}

		private void resolveContext(final PsiElement context)
		{
			parentContainer = PsiTreeUtil.getParentOfType(context, JSFunction.class, JSClass.class);

			if(parentContainer instanceof JSFunctionExpression)
			{
				boolean changedFunctionScope = false;

				while(parentContainer instanceof JSFunctionExpression)
				{
					final PsiElement parentContainerParent = parentContainer.getParent();

					if(parentContainerParent instanceof JSAssignmentExpression)
					{
						final JSExpression jsExpression = ((JSAssignmentExpression) parentContainerParent).getLOperand();
						final JSElement functionExpressionName = jsExpression instanceof JSDefinitionExpression ? ((JSDefinitionExpression) jsExpression).getExpression() : null;
						qualifyingExpression = functionExpressionName;

						if(functionExpressionName instanceof JSReferenceExpression)
						{
							final JSExpression functionExpressionNameQualifier = ((JSReferenceExpression) functionExpressionName).getQualifier();

							if(functionExpressionNameQualifier instanceof JSThisExpression)
							{
								parentContainer = PsiTreeUtil.getParentOfType(functionExpressionName, JSFunction.class);
								qualifyingExpression = null;
								changedFunctionScope = true;
								continue;
							}
							else if(functionExpressionNameQualifier instanceof JSReferenceExpression)
							{
								final String functionExpressionNameQualifierText = ((JSReferenceExpression) functionExpressionNameQualifier).getReferencedName();

								if(PROTOTYPE_FIELD_NAME.equals(functionExpressionNameQualifierText))
								{
									qualifyingExpression = ((JSReferenceExpression) functionExpressionNameQualifier).getQualifier();
								}
								else if(!changedFunctionScope)
								{
									String referencedName;

									if(((JSReferenceExpression) functionExpressionNameQualifier).getQualifier() == null && ((referencedName = ((JSReferenceExpression) functionExpressionName)
											.getReferencedName()) == null ||
											referencedName.length() == 0 ||
											!Character.isUpperCase(referencedName.charAt(0))))
									{
										qualifyingExpression = functionExpressionNameQualifier;
									}
								}
							}
						}
					}
					else if(parentContainerParent instanceof JSProperty)
					{
						final JSElement element = PsiTreeUtil.getParentOfType(parentContainerParent, JSVariable.class, JSAssignmentExpression.class, JSArgumentList.class);
						if(element instanceof JSVariable)
						{
							qualifyingExpression = element;
						}
						else if(element instanceof JSAssignmentExpression)
						{
							JSExpression loperand = ((JSAssignmentExpression) element).getLOperand();
							if(loperand != null)
							{
								qualifyingExpression = ((JSDefinitionExpression) loperand).getExpression();
							}
						}
						else if(element instanceof JSArgumentList)
						{
							qualifyingExpression = JSSymbolUtil.findQualifyingExpressionFromArgumentList((JSArgumentList) element);
						}
					}
					else if(parentContainerParent instanceof JSNewExpression)
					{
						final JSElement element = PsiTreeUtil.getParentOfType(parentContainerParent, JSVariable.class, JSAssignmentExpression.class, JSArgumentList.class);

						if(element instanceof JSVariable)
						{
							qualifyingExpression = element;
						}
						else if(element instanceof JSAssignmentExpression)
						{
							qualifyingExpression = ((JSDefinitionExpression) ((JSAssignmentExpression) element).getLOperand()).getExpression();
						}
					}
					else if(parentContainerParent instanceof JSReferenceExpression)
					{
						parentContainer = PsiTreeUtil.getParentOfType(parentContainerParent, JSFunction.class);
						continue;
					}
					else if(parentContainer.getName() == null)
					{
						parentContainer = PsiTreeUtil.getParentOfType(parentContainerParent, JSFunction.class);
						continue;
					}

					break;
				}

				if(qualifyingExpression instanceof JSReferenceExpression)
				{
					final JSReferenceExpression qualifyingReferenceExpression = ((JSReferenceExpression) qualifyingExpression);
					final String functionExpressionNameQualifierText = qualifyingReferenceExpression.getReferencedName();

					if(PROTOTYPE_FIELD_NAME.equals(functionExpressionNameQualifierText))
					{
						qualifyingExpression = qualifyingReferenceExpression.getQualifier();
					}
					else
					{

						qualifyingExpression = JSSymbolUtil.findReferenceExpressionUsedForClassExtending(qualifyingReferenceExpression);
					}
				}
			}

			PsiElement parentContainerParent = parentContainer != null ? parentContainer.getParent() : null;

			if(parentContainerParent instanceof JSFile)
			{
				parentContainerParent = getClassReferenceForXmlFromContext(parentContainerParent);
			}

			if(parentContainerParent instanceof JSClass)
			{
				parentContainer = (JSNamedElement) parentContainerParent;
			}

			if(parentContainer instanceof JSFunctionExpression && parentContainer.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4)
			{
				parentContainer = null;
				qualifyingExpression = null;
				typeName = OBJECT_CLASS_NAME;
			}
		}

		public
		@Nullable
		String getQualifierAsString()
		{
			String qualifierAsString = getOriginalQualifierAsString();
			return normalizeQualifier(qualifierAsString);
		}

		private static String normalizeQualifier(String qualifierAsString)
		{
			if("Window".equals(qualifierAsString) || "Document".equals(qualifierAsString))
			{
				qualifierAsString = StringUtil.decapitalize(qualifierAsString);
			}
			return qualifierAsString;
		}

		private String getOriginalQualifierAsString()
		{
			if(qualifyingExpression instanceof JSLiteralExpression)
			{
				return StringUtil.stripQuotesAroundValue(qualifyingExpression.getText());
			}
			else if(qualifyingExpression instanceof JSReferenceExpression)
			{
				return qualifyingExpression.getText();
			}
			else if(qualifyingExpression instanceof JSNamedElement)
			{
				return qualifyingExpression.getName();
			}
			else if(parentContainer instanceof JSQualifiedNamedElement)
			{
				return ((JSQualifiedNamedElement) parentContainer).getQualifiedName();
			}

			return typeName;
		}

		public
		@Nullable
		int[] getQualifierAsNameIndex(@NotNull JavaScriptIndex index)
		{
			String qualifierAsString = getQualifierAsString();
			if(qualifierAsString != null)
			{
				TIntArrayList tIntArrayList = new TIntArrayList();
				qualifierAsString = BaseJSSymbolProcessor.addIndexListFromQName(qualifierAsString, qualifyingExpression, tIntArrayList, index);
				return tIntArrayList.toNativeArray();
			}

			return null;
		}

		public static String getQualifierOfExprAsString(JSElement expr)
		{
			if(expr instanceof JSNamedElement)
			{
				return expr.getName();
			}
			if(expr instanceof JSReferenceExpression && PROTOTYPE_FIELD_NAME.equals(((JSReferenceExpression) expr).getReferencedName()))
			{
				expr = ((JSReferenceExpression) expr).getQualifier();
			}
			return normalizeQualifier(StringUtil.stripQuotesAroundValue(expr.getText()));
		}
	}

	private static boolean isInjectedFile(PsiElement element)
	{
		return element instanceof PsiFile && (((PsiFile) element).getVirtualFile() instanceof LightVirtualFile);
	}

	public interface OverrideHandler
	{
		boolean process(ResolveProcessor processor, final PsiElement scope, String className);
	}

	public interface MetaDataProcessor
	{
		boolean process(final @NotNull JSAttribute jsAttribute);

		boolean handleOtherElement(final PsiElement el, PsiElement context, @Nullable Ref<PsiElement> continuePassElement);
	}

	public static abstract class CollectMethodsToImplementProcessor extends ResolveProcessor
	{
		private final String myName;

		public CollectMethodsToImplementProcessor(final String name, PsiElement context)
		{
			super(null, context);
			myName = name;

			setToSkipClassDeclarationsOnce(true);
			setToProcessHierarchy(true);
			setToProcessMembers(false);
			setTypeContext(true);
		}

		@Override
		public boolean execute(final PsiElement element, final ResolveState state)
		{
			final ResolveProcessor processor = new ResolveProcessor(myName, place)
			{
				@Override
				public boolean execute(final PsiElement element, final ResolveState state)
				{
					if(element instanceof JSFunction)
					{
						if(OBJECT_CLASS_NAME.equals(((JSClass) findParent((JSNamedElement) element)).getQualifiedName()) || ((JSFunction) element).isConstructor())
						{
							return true;
						}
					}
					else
					{
						return true;
					}
					return super.execute(element, state);
				}

				{
					setToProcessHierarchy(true);
				}
			};
			for(JSClass implementedInterface : ((JSClass) element).getImplementedInterfaces())
			{
				if(!implementedInterface.isInterface())
				{
					continue;
				}
				final boolean b = implementedInterface.processDeclarations(processor, ResolveState.initial(), implementedInterface, implementedInterface);

				if(!b)
				{
					process(processor);
					break;
				}
			}

			if(myName == null && processor.getResults() != null)
			{
				process(processor);
			}
			return false;
		}

		protected abstract boolean process(ResolveProcessor processor);
	}

	static JSSourceElement[] getSourceElements(final PsiElement owner)
	{
		if(owner instanceof JSStubElementImpl)
		{
			return (JSSourceElement[]) ((JSStubElementImpl) owner).getStubOrPsiChildren(JSElementTypes.SOURCE_ELEMENTS, JSSourceElement.EMPTY_ARRAY);
		}
		else if(owner instanceof JSFile)
		{
			return ((JSFile) owner).getStatements();
		}
		return JSSourceElement.EMPTY_ARRAY;
	}

	public static PsiElement[] getStubbedChildren(final PsiElement owner)
	{
		return getStubbedChildren(owner, ourStubbedFilter);
	}

	public static PsiElement[] getStubbedChildren(final PsiElement owner, TokenSet filter)
	{
		if(owner instanceof JSStubElementImpl)
		{
			final JSStubElementImpl element = (JSStubElementImpl) owner;
			final StubElement stub = element.getStub();
			if(stub != null)
			{
				return stub.getChildrenByType(filter, PsiElement.EMPTY_ARRAY);
			}
			return getChildrenFromTokenSet(element.getNode(), filter);
		}
		else if(owner instanceof JSFile)
		{
			final JSFile file = (JSFile) owner;
			final StubElement stub = ((JSFileImpl) file).getStub();
			if(stub != null)
			{
				return stub.getChildrenByType(filter, PsiElement.EMPTY_ARRAY);
			}
			return getChildrenFromTokenSet(file.getNode(), filter);
		}
		return PsiElement.EMPTY_ARRAY;
	}

	private static PsiElement[] getChildrenFromTokenSet(final ASTNode astNode, TokenSet filter)
	{
		final ASTNode[] astNodes = astNode.getChildren(filter);
		PsiElement[] result = new PsiElement[astNodes.length];
		for(int i = 0; i < astNodes.length; ++i)
		{
			result[i] = astNodes[i].getPsi();
		}
		return result;
	}

	private static final TokenSet ourStubbedFilter = TokenSet.create(JSElementTypes.CLASS, JSElementTypes.VAR_STATEMENT, JSElementTypes.FUNCTION_DECLARATION, JSElementTypes.ATTRIBUTE_LIST,
			JSElementTypes.INCLUDE_DIRECTIVE, JSElementTypes.ATTRIBUTE, JSElementTypes.PACKAGE_STATEMENT);

	private static class ImplicitJSVariableImpl extends LightElement implements JSVariable
	{
		private final PsiFile myContainingFile;
		private final String myName;
		private final String myType;

		public ImplicitJSVariableImpl(final String name, String qName, PsiFile containingFile)
		{
			super(containingFile.getManager(), JavaScriptSupportLoader.ECMA_SCRIPT_L4.getBaseLanguage());
			myContainingFile = containingFile;
			myName = name;
			myType = qName;
		}

		@Override
		public JSAttributeList getAttributeList()
		{
			return null;
		}

		@Override
		public boolean isValid()
		{
			return myContainingFile.isValid();
		}

		@Override
		public PsiElement getParent()
		{
			return myContainingFile;
		}

		@Override
		public PsiFile getContainingFile()
		{
			return myContainingFile;
		}

		@Override
		public String getText()
		{
			return null;
		}

		@Override
		public void accept(@NotNull PsiElementVisitor visitor)
		{
			if(visitor instanceof JSElementVisitor)
			{
				((JSElementVisitor) visitor).visitJSVariable(this);
			}
			else
			{
				visitor.visitElement(this);
			}
		}

		@Override
		public PsiElement copy()
		{
			return new ImplicitJSVariableImpl(myName, myType, myContainingFile);
		}

		@Override
		public boolean canNavigate()
		{
			return false;
		}

		@Override
		public boolean isPhysical()
		{
			return false;
		}

		@NotNull
		@Override
		public ASTNode getNode()
		{
			return null;
		}

		@Override
		public IStubElementType getElementType()
		{
			return JSElementTypes.VARIABLE;
		}

		@Override
		public JSVariableStubBase getStub()
		{
			return null;
		}

		@Override
		public boolean hasInitializer()
		{
			return false;
		}

		@Override
		public JSExpression getInitializer()
		{
			return null;
		}

		@Override
		public String getInitializerText()
		{
			return null;
		}

		@Override
		public void setInitializer(JSExpression expr) throws IncorrectOperationException
		{
		}

		@Override
		public JSType getType()
		{
			return null;
		}

		@Override
		public String getTypeString()
		{
			return myType;
		}

		@Override
		public PsiElement getTypeElement()
		{
			return null;
		}

		@Override
		public boolean isConst()
		{
			return false;
		}

		@Override
		public boolean isLocal()
		{
			return false;
		}

		@Override
		public boolean isDeprecated()
		{
			return false;
		}

		@Override
		public String getQualifiedName()
		{
			return myName;
		}

		@Override
		public ASTNode findNameIdentifier()
		{
			return null;
		}

		@Override
		public String getName()
		{
			return myName;
		}

		@Override
		public PsiElement setName(@NonNls @NotNull String name) throws IncorrectOperationException
		{
			throw new IncorrectOperationException();
		}

		@Override
		public PsiElement getNameIdentifier()
		{
			return null;
		}

		@Override
		public String toString()
		{
			return "js_implicit_var:" + myName + ", type:" + myType + ", file:" + myContainingFile;
		}

		@Override
		public boolean isEquivalentTo(PsiElement another)
		{
			return another == this || (another instanceof ImplicitJSVariableImpl &&
					myContainingFile == ((ImplicitJSVariableImpl) another).myContainingFile &&
					myName.equals(((ImplicitJSVariableImpl) another).myName));
		}
	}
}
