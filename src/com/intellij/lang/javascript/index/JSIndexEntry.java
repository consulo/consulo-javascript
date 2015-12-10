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

package com.intellij.lang.javascript.index;

import static com.intellij.lang.javascript.index.JSNamedElementProxy.NamedItemType;

import gnu.trove.THashMap;
import gnu.trove.TObjectHashingStrategy;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.*;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.MultiplePsiFilesPerDocumentFileViewProvider;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttribute;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.reference.SoftReference;

/**
 * @by yole, maxim.mossienko
 */
public class JSIndexEntry
{
	protected final VirtualFile myFile;
	private SoftReference<PsiFile> myPsiFileReference;
	private Language myLanguage;
	protected final JavaScriptIndex myIndex;
	@NonNls
	private static final String DEPRECATED_TAG = "deprecated";
	private boolean myContentBelongsOnlyToMyFile;

	private static final ProxyProcessor<Set<String>> myCollectClassNamesProcessor = new ProxyProcessor<Set<String>>()
	{
		@Override
		public boolean accepts(final PsiFile psiFile)
		{
			return true;
		}

		@Override
		public void process(final JSNamedElementProxy proxy, JavaScriptIndex index, final Set<String> classNames)
		{
			if(proxy.getType() == NamedItemType.Clazz)
			{
				classNames.add(proxy.getNameId());
			}
		}
	};

	private final static ProxyProcessor<Set<NavigationItem>> myFindClassesByName = new ProxyProcessor<Set<NavigationItem>>()
	{
		@Override
		public boolean accepts(final PsiFile psiFile)
		{
			return !JavaScriptIndex.isFromPredefinedFile(psiFile) || ApplicationManager.getApplication().isUnitTestMode();
		}

		@Override
		public void process(final JSNamedElementProxy proxy, final JavaScriptIndex index, final Set<NavigationItem> param)
		{
			if(proxy.getType() == NamedItemType.Clazz)
			{
				param.add(proxy);
			}
		}
	};

	private static final ProxyProcessor<Set<String>> myCollectSymbolNamesProcessor = new ProxyProcessor<Set<String>>()
	{
		@Override
		public boolean accepts(final PsiFile psiFile)
		{
			return true;
		}

		@Override
		public void process(final JSNamedElementProxy proxy, final JavaScriptIndex index, final Set<String> param)
		{
			if(proxy.getType() != NamedItemType.Clazz)
			{
				param.add(proxy.getNameId());
			}
		}
	};

	private static final ProxyProcessor<Set<NavigationItem>> myFindSymbolsByNameProcessor = new ProxyProcessor<Set<NavigationItem>>()
	{
		@Override
		public boolean accepts(final PsiFile psiFile)
		{
			return !JavaScriptIndex.isFromPredefinedFile(psiFile) || ApplicationManager.getApplication().isUnitTestMode();
		}

		@Override
		public void process(final JSNamedElementProxy proxy, final JavaScriptIndex index, final Set<NavigationItem> param)
		{
			if(proxy.getType() != NamedItemType.Clazz)
			{
				param.add(proxy);
			}
		}
	};

	public void setContentBelongsOnlyToMyFile()
	{
		myContentBelongsOnlyToMyFile = true;
		invalidate();
	}

	public long getTimeStamp()
	{
		return myFile.getTimeStamp();
	}

	public void fillClassNames(final Set<String> classNames)
	{
		process(null, myCollectClassNamesProcessor, classNames);
	}

	public void fillClassByName(final String name, final Set<NavigationItem> classes)
	{
		process(name, myFindClassesByName, classes);
	}

	static class IndexEntryContent
	{
		private final Map<String, Object> mySymbols = new HashMap<String, Object>();
		private final Map<JSNamedElement, JSNamespace> mySymbolNameComponents = new THashMap<JSNamedElement, JSNamespace>(TObjectHashingStrategy.IDENTITY);
		private final JSRootNamespace myNamespace;
		private long timestamp;

		IndexEntryContent(Project project, JSIndexEntry entry)
		{
			myNamespace = new JSRootNamespace(JavaScriptIndex.getInstance(project).getDefaultPackage(), entry);
		}
	}

	private CachedValue<IndexEntryContent> myIndexValue;
	private IndexEntryContent myContent;


	public JSIndexEntry(final @Nullable VirtualFile constructionData, Project project, boolean lazy, Language language)
	{
		myIndex = JavaScriptIndex.getInstance(project);
		myFile = constructionData;
		myLanguage = language;
		doInitFor(null);
		if(!lazy)
		{
			myIndexValue.getValue();
		}
	}

	boolean isUpToDate()
	{
		final PsiFile file = getFile();
		return myContent != null && myContent.timestamp == file.getModificationStamp() && file.isValid();
	}

	private static void doAddNamedItemProxy(final String nameId, final JSNamedElement myElement, final boolean toCheckUniqueness,
			final JSNamespace namespace, IndexEntryContent myContent)
	{
		final Object o = myContent.mySymbols.get(nameId);
		if(o == null)
		{
			myContent.mySymbols.put(nameId, myElement);
		}
		else if(o instanceof JSNamedElement)
		{
			if(toCheckUniqueness && myContent.mySymbolNameComponents.get((JSNamedElement) o) == namespace)
			{
				if(((MyJSNamedItem) o).getType() == NamedItemType.Definition)
				{
					return;
				}
			}
			myContent.mySymbols.put(nameId, new Object[]{
					o,
					myElement
			});
		}
		else
		{
			final Object[] oArray = (Object[]) o;

			if(toCheckUniqueness)
			{
				for(Object oE : oArray)
				{
					if(myContent.mySymbolNameComponents.get((JSNamedElement) oE) == namespace)
					{
						if(((MyJSNamedItem) oE).getType() == NamedItemType.Definition)
						{
							return;
						}
					}
				}
			}

			Object[] newArray = new Object[oArray.length + 1];
			System.arraycopy(oArray, 0, newArray, 0, oArray.length);
			newArray[oArray.length] = myElement;
			myContent.mySymbols.put(nameId, newArray);
		}

		myContent.mySymbolNameComponents.put(myElement, namespace);
	}

	private void doInitFor(final IndexEntryContent content)
	{
		final PsiFile psiFile = myFile != null ? buildPsiFileFromFile() : null;
		assert psiFile != null || content != null : "Psi file could not be retrieved for " + myFile;

		myIndexValue = CachedValuesManager.getManager(myIndex.getProject()).createCachedValue(new CachedValueProvider<IndexEntryContent>()
		{
			boolean computeFirstTime = true;
			final ModificationTracker tracker = new ModificationTracker()
			{
				@Override
				public long getModificationCount()
				{
					return myFile != null ? getFile().getModificationStamp() : -1;
				}
			};

			@Override
			public Result<IndexEntryContent> compute()
			{
				final PsiFile psiFile = myFile != null ? getFile() : null;

				if(computeFirstTime)
				{
					computeFirstTime = false;

					if(content != null)
					{ // loaded from disk
						myContent = content;
						myContent.timestamp = psiFile != null ? psiFile.getModificationStamp() : -1;
						return new Result<IndexEntryContent>(myContent, tracker);
					}
				}

				final Project project = myIndex.getProject();
				if(psiFile == null)
				{ // virtual file become invalid?
					myContent = new IndexEntryContent(project, JSIndexEntry.this);
					return new Result<IndexEntryContent>(myContent);
				}

				synchronized(myIndex)
				{
					if(myContent == null)
					{
						myContent = new IndexEntryContent(project, JSIndexEntry.this);
						updateFromTree(psiFile);
					}
					else if(myContent.timestamp != psiFile.getModificationStamp())
					{
						if(!myContentBelongsOnlyToMyFile)
						{
							invalidate();
						}
						myContent.myNamespace.clear();
						myContent.mySymbolNameComponents.clear();
						myContent.mySymbols.clear();

						updateFromTree(psiFile);
					}
				}

				return new Result<IndexEntryContent>(myContent, tracker);
			}

			private void updateFromTree(final PsiFile psiFile)
			{
				JSSymbolUtil.visitSymbols(psiFile, myContent.myNamespace, new JSSymbolUtil.JavaScriptSymbolProcessorEx()
				{
					private JSSymbolUtil.JavaScriptSymbolProcessingHost myProcessingHost;

					@Override
					public boolean processFunction(JSNamespace namespace, final String nameId, JSNamedElement function)
					{
						final PsiElement parent = function.getParent();
						addSymbol(nameId, function, parent instanceof JSClass ? NamedItemType.MemberFunction : function instanceof JSFunctionExpression ? (parent
								instanceof JSProperty ? NamedItemType.FunctionProperty : NamedItemType.FunctionExpression) : NamedItemType.Function, namespace);
						return true;
					}

					@Override
					public boolean processClass(final JSNamespace namespace, final String nameId, final JSNamedElement clazz)
					{
						addSymbol(nameId, clazz, NamedItemType.Clazz, namespace);
						return true;
					}

					@Override
					public boolean processNamespace(final JSNamespace namespace, final String nameId, final JSNamedElement ns)
					{
						addSymbol(nameId, ns, NamedItemType.Namespace, namespace);
						return true;
					}

					@Override
					public boolean processImplicitNamespace(final JSNamespace namespace, final String nameId, final PsiElement refExpr, boolean finalReference)
					{
						addSymbol(nameId, refExpr, NamedItemType.Namespace, namespace, finalReference);
						return true;
					}

					@Override
					public boolean processImplicitFunction(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
					{
						addSymbol(nameId, refExpr, NamedItemType.ImplicitFunction, namespace);
						return true;
					}

					@Override
					public boolean processImplicitVariable(final JSNamespace namespace, final String nameId, final PsiElement refExpr)
					{
						addSymbol(nameId, refExpr, NamedItemType.ImplicitVariable, namespace);
						return true;
					}

					final JSTypeEvaluateManager typeEvaluateManager = JSTypeEvaluateManager.getInstance(myIndex.getProject());

					private final void addSymbol(final String nameId, final PsiElement element, NamedItemType type, JSNamespace namespace)
					{
						addSymbol(nameId, element, type, namespace, true);
					}

					private final void addSymbol(final String nameId, final PsiElement element, NamedItemType type, JSNamespace namespace, boolean finalReference)
					{
						if(nameId == null)
						{
							return;
						}

						final JSNamedElement myElement = new MyJSNamedItem(JSIndexEntry.this, element.getTextOffset(), nameId, type);
						final MyJSNamedItem proxy = (MyJSNamedItem) myElement;

						if(finalReference)
						{
							proxy.setDeprecated(myProcessingHost.isCurrentItemDeprecated());
							proxy.setAccessType(myProcessingHost.getAccessType());
							//final String typeString = myProcessingHost.getCurrentItemType();
							myProcessingHost.resetState();
						}

						if((type == NamedItemType.MemberFunction ||
								type == NamedItemType.Function ||
								type == NamedItemType.FunctionExpression ||
								type == NamedItemType.FunctionProperty) && element instanceof JSFunction)
						{
							final JSFunction function = (JSFunction) element;
							String s = function.getReturnTypeString();

							if(function.isGetProperty())
							{
								proxy.setProperty(JSNamedElementProxy.Property.GetFunction, true);
							}
							if(function.isSetProperty())
							{
								proxy.setProperty(JSNamedElementProxy.Property.SetFunction, true);
							}
							if(function.isConstructor())
							{
								proxy.setProperty(JSNamedElementProxy.Property.Constructor, true);
								s = null;
							}

							if(s != null && s.length() > 0)
							{
								typeEvaluateManager.setElementType(myElement, s);
							}

							saveModifiersFromAttrList(proxy, function);
						}
						else if((type == NamedItemType.Variable || type == NamedItemType.MemberVariable) && element instanceof JSVariable)
						{
							final JSVariable variable = (JSVariable) element;
							final String s = variable.getTypeString();
							if(s != null && s.length() > 0)
							{
								typeEvaluateManager.setElementType(myElement, s);
							}
							saveModifiersFromAttrList(proxy, variable);
						}
						else if(type == NamedItemType.Definition && element instanceof JSDefinitionExpression || type == NamedItemType.ImplicitFunction)
						{
							final String s = JSDocumentationUtils.findType(element);
							if(s != null && s.length() > 0)
							{
								typeEvaluateManager.setElementType(myElement, s);
							}
						}
						else if(type == NamedItemType.Clazz)
						{
							final JSClass clazz = (JSClass) element;
							final JSAttributeList attributeList = clazz.getAttributeList();

							if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC))
							{
								proxy.setProperty(JSNamedElementProxy.Property.Dynamic, true);
							}
							if(clazz.isInterface())
							{
								proxy.setProperty(JSNamedElementProxy.Property.Interface, true);
							}
							if(clazz.findFunctionByName(clazz.getName()) != null)
							{
								proxy.setProperty(JSNamedElementProxy.Property.HasConstructor, true);
							}
						}

						boolean toCheckUniqueness = element instanceof JSDefinitionExpression;

						if(toCheckUniqueness)
						{
							JSExpression expression = ((JSDefinitionExpression) element).getExpression();
							while(expression instanceof JSReferenceExpression)
							{
								expression = ((JSReferenceExpression) expression).getQualifier();
							}

							toCheckUniqueness = expression instanceof JSThisExpression;
						}

						doAddNamedItemProxy(nameId, myElement, toCheckUniqueness, namespace, myContent);
					}

					@Override
					public boolean processVariable(JSNamespace namespace, final String nameId, JSNamedElement variable)
					{
						PsiElement parent = variable.getParent();
						if(parent instanceof JSVarStatement)
						{
							parent = parent.getParent();
						}

						addSymbol(nameId, variable, parent instanceof JSClass ? NamedItemType.MemberVariable : NamedItemType.Variable, namespace);
						return true;
					}

					@Override
					public boolean acceptsFile(PsiFile file)
					{
						return true;
					}

					@Override
					public PsiFile getBaseFile()
					{
						return null;
					}

					@Override
					public boolean processProperty(final JSNamespace namespace, final String nameId, final JSNamedElement property)
					{
						addSymbol(nameId, property, NamedItemType.Property, namespace);
						return true;
					}

					@Override
					public boolean processDefinition(final JSNamespace namespace, final String nameId, final JSNamedElement expression)
					{
						//if (namespace.length > 0) {
						final JSDefinitionExpression element = (JSDefinitionExpression) expression;
						addSymbol(nameId, element, NamedItemType.Definition, namespace);
						//}
						return true;
					}

					@Override
					@Nullable
					public String getRequiredNameId()
					{
						return null;
					}

					@Override
					public boolean processTag(JSNamespace namespace, final String nameId, PsiNamedElement namedElement, final String attrName)
					{
						if(psiFile instanceof XmlFile)
						{
							final XmlAttribute attribute = ((XmlTag) namedElement).getAttribute(attrName, null);
							final XmlAttributeValue xmlAttributeValue = attribute != null ? attribute.getValueElement() : null;
							final PsiElement[] chidren = xmlAttributeValue != null ? xmlAttributeValue.getChildren() : PsiElement.EMPTY_ARRAY;

							if(chidren.length == 3)
							{
								addSymbol(nameId, chidren[1], NamedItemType.AttributeValue, namespace);
							}
						}
						return true;
					}

					@Override
					public void setProcessingHost(final JSSymbolUtil.JavaScriptSymbolProcessingHost processingHost)
					{
						myProcessingHost = processingHost;
					}
				});
				myContent.timestamp = psiFile.getModificationStamp();
				if(!myContentBelongsOnlyToMyFile)
				{
					myContent.myNamespace.validate();
				}
			}
		}, false);

		if(content != null)
		{
			myIndexValue.getValue();
		}
	}

	private static void saveModifiersFromAttrList(final MyJSNamedItem proxy, final JSAttributeListOwner function)
	{
		final JSAttributeList attributeList = function.getAttributeList();
		if(attributeList != null)
		{
			if(attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE))
			{
				proxy.setProperty(JSNamedElementProxy.Property.Override, true);
			}
			if(attributeList.hasModifier(JSAttributeList.ModifierType.STATIC))
			{
				proxy.setProperty(JSNamedElementProxy.Property.Static, true);
			}
		}
	}

	protected PsiFile buildPsiFileFromFile()
	{
		//if (myFile.get)
		PsiFile psifile = PsiManager.getInstance(myIndex.getProject()).findFile(myFile);
		FileViewProvider fileViewProvider;

		if(psifile != null && (fileViewProvider = psifile.getViewProvider()) instanceof MultiplePsiFilesPerDocumentFileViewProvider)
		{
			assert myLanguage != null;
			psifile = fileViewProvider.getPsi(myLanguage);
		}
		return psifile;
	}

	interface ProxyProcessor<T>
	{
		boolean accepts(final PsiFile psiFile);

		void process(JSNamedElementProxy proxy, JavaScriptIndex index, T param);
	}

	private <T> void process(String requiredFieldId, ProxyProcessor<T> processor, T param)
	{
		PsiFile myPsiFile = getFile();
		if(myPsiFile == null || !processor.accepts(myPsiFile))
		{
			return;
		}

		final IndexEntryContent index = myIndexValue.getValue();

		if(requiredFieldId != null)
		{
			final Object value = index.mySymbols.get(requiredFieldId);
			if(value instanceof Object[])
			{
				for(Object o : (Object[]) value)
				{
					processor.process((JSNamedElementProxy) o, myIndex, param);
				}
			}
			else if(value != null)
			{
				processor.process((JSNamedElementProxy) value, myIndex, param);
			}
		}
		else
		{
			for(Map.Entry<JSNamedElement, JSNamespace> e : index.mySymbolNameComponents.entrySet())
			{
				processor.process((JSNamedElementProxy) e.getKey(), myIndex, param);
			}
		}
	}

	void fillSymbolNames(final Set<String> symbolNames)
	{
		process(null, myCollectSymbolNamesProcessor, symbolNames);
	}

	void fillSymbolsByName(final String name, final Set<NavigationItem> symbolNavItems)
	{
		process(name, myFindSymbolsByNameProcessor, symbolNavItems);
	}

	public void processSymbols(JavaScriptSymbolProcessor processor)
	{
		synchronized(myIndex)
		{
			processSymbolsNoLock(processor);
		}
	}

	void processSymbolsNoLock(JavaScriptSymbolProcessor processor)
	{
		ProgressManager.getInstance().checkCanceled();

		PsiFile psiFile = myFile != null && myFile.isValid() ? getFile() : null;
		if(psiFile == null || !psiFile.isValid())
		{
			return;
		}
		final JSIndexEntry.IndexEntryContent index = myIndexValue.getValue();

		if(!processor.acceptsFile(psiFile))
		{
			return;
		}

		final String requiredNameId = processor.getRequiredNameId();
		if(requiredNameId != null)
		{
			final Object value = index.mySymbols.get(requiredNameId);

			if(value instanceof Object[])
			{
				for(Object item : (Object[]) value)
				{
					if(!dispatchProcessorCall(index, (JSNamedElementProxy) item, processor))
					{
						return;
					}
				}
			}
			else if(value != null)
			{
				if(!dispatchProcessorCall(index, (JSNamedElementProxy) value, processor))
				{
					return;
				}
			}

			//dispatchProcessorForNamespaces();
		}
		else
		{ // full scan
			for(Map.Entry<String, Object> entry : index.mySymbols.entrySet())
			{
				final Object value = entry.getValue();

				if(value instanceof Object[])
				{
					for(Object item : (Object[]) value)
					{
						if(!dispatchProcessorCall(index, (JSNamedElementProxy) item, processor))
						{
							return;
						}
					}
				}
				else
				{
					if(!dispatchProcessorCall(index, (JSNamedElementProxy) value, processor))
					{
						return;
					}
				}
			}
		}
	}

	private static boolean dispatchProcessorCall(final IndexEntryContent index, final JSNamedElementProxy item,
			final JavaScriptSymbolProcessor processor)
	{
		final JSNamespace namespace = index.mySymbolNameComponents.get(item);

		final NamedItemType itemType = item.getType();
		final String nameId = item.getNameId();

		if(itemType == NamedItemType.Variable || itemType == NamedItemType.MemberVariable)
		{
			return processor.processVariable(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.Function ||
				itemType == NamedItemType.FunctionExpression ||
				itemType == NamedItemType.FunctionProperty ||
				itemType == NamedItemType.MemberFunction)
		{
			return processor.processFunction(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.Property)
		{
			return processor.processProperty(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.Definition)
		{
			return processor.processDefinition(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.AttributeValue)
		{
			return processor.processTag(namespace, nameId, item, null);
		}
		else if(itemType == NamedItemType.Clazz)
		{
			return processor.processClass(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.Namespace)
		{
			return processor.processNamespace(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.ImplicitFunction)
		{
			return processor.processImplicitFunction(namespace, nameId, item);
		}
		else if(itemType == NamedItemType.ImplicitVariable)
		{
			return processor.processImplicitVariable(namespace, nameId, item);
		}
		else
		{
			assert false : "Invalid type:" + itemType;
		}
		return true;
	}

	public void initTypesAndBrowserSpecifics()
	{
		final BrowserSupportManager browserSupportManager = BrowserSupportManager.getInstance(myIndex.getProject());
		final JSTypeEvaluateManager typeEvaluateManager = JSTypeEvaluateManager.getInstance(myIndex.getProject());

		for(JSNamedElement el : myIndexValue.getValue().mySymbolNameComponents.keySet())
		{
			JSElement realElement = PsiTreeUtil.getParentOfType(((JSNamedElementProxy) el).getElement(), JSStatement.class);
			if(realElement == null)
			{
				continue;
			}
			final PsiElement lastChild = realElement.getNextSibling();

			if(lastChild instanceof PsiComment)
			{
				final String s = lastChild.getText().substring(2);
				final int typeIndex = s.indexOf(',');
				final int deprecatedIndex = s.indexOf(',', typeIndex + 1);
				final boolean deprecated = s.endsWith(DEPRECATED_TAG);
				final String browserType = typeIndex != -1 ? s.substring(0, typeIndex).toLowerCase() : "";

				if(browserType.equals("ie"))
				{
					browserSupportManager.addIESpecificSymbol(el);
				}
				else if(browserType.equals("gecko"))
				{
					browserSupportManager.addGeckoSpecificSymbol(el);
				}
				else if(browserType.equals("opera"))
				{
					browserSupportManager.addOperaSpecificSymbol(el);
				}

				((MyJSNamedItem) el).setDeprecated(deprecated);
				typeEvaluateManager.setElementType(el, s.substring(typeIndex + 1, deprecatedIndex != -1 ? deprecatedIndex : s.length()));
			}
		}
	}

	public static void encodeBrowserSpecificsAndType(StringBuilder builder, String browserSpecific, String type, boolean deprecated)
	{
		if(type == null)
		{
			type = "Object";
		}
		builder.append("//");
		final int length = builder.length();

		if(browserSpecific != null)
		{
			builder.append(browserSpecific);
		}
		if(type != null)
		{
			if(length != builder.length())
			{
				builder.append(',');
			}
			builder.append(type);
		}

		if(deprecated)
		{
			if(length != builder.length())
			{
				builder.append(',');
			}
			builder.append(DEPRECATED_TAG);
		}
	}

	public final
	@Nullable
	PsiFile getFile()
	{
		PsiFile psiFile = myPsiFileReference != null ? myPsiFileReference.get() : null;
		if(psiFile == null && myFile != null && myFile.isValid())
		{
			psiFile = buildPsiFileFromFile();
			myPsiFileReference = new SoftReference<PsiFile>(psiFile);
		}

		return psiFile;
	}

	JSNamespace getNamespace(final JSNamedElement myJSNamedItem)
	{
		synchronized(myIndex)
		{
			return myIndexValue.getValue().mySymbolNameComponents.get(myJSNamedItem);
		}
	}

	void invalidate()
	{
		invalidate(myIndex.getProject());
	}

	@Nullable
	VirtualFile getVirtualFile()
	{
		return myFile;
	}

	void invalidate(@NotNull Project project)
	{
		synchronized(JavaScriptIndex.getInstance(project))
		{
			if(myContent == null)
			{
				return; // no one ever cared about it
			}

			final JSTypeEvaluateManager typeEvaluateManager = JSTypeEvaluateManager.getInstance(project);
			for(Map.Entry<JSNamedElement, JSNamespace> entry : myContent.mySymbolNameComponents.entrySet())
			{
				typeEvaluateManager.removeElementInfo(entry.getKey());
			}
			myContent.myNamespace.invalidate(typeEvaluateManager);
		}
	}

	JSNamespace getTopLevelNsNoLock()
	{
		return myIndexValue.getValue().myNamespace;
	}

	public JSNamespace getTopLevelNs()
	{
		synchronized(myIndex)
		{
			return getTopLevelNsNoLock();
		}
	}

	public boolean processSymbolsInNs(final @NotNull JavaScriptSymbolProcessor myProcessor, final @NotNull JSNamespace jsNamespace)
	{
		PsiFile myPsiFile = getFile();
		if(myPsiFile == null)
		{
			return true;
		}
		if(!myProcessor.acceptsFile(myPsiFile))
		{
			return true;
		}

		synchronized(myIndex)
		{
			final IndexEntryContent index = myIndexValue.getValue();
			final String requiredNameId = myProcessor.getRequiredNameId();

			if(requiredNameId == null)
			{
				for(Map.Entry<JSNamedElement, JSNamespace> e : index.mySymbolNameComponents.entrySet())
				{
					if(e.getValue() == jsNamespace)
					{
						if(!dispatchProcessorCall(index, (JSNamedElementProxy) e.getKey(), myProcessor))
						{
							return false;
						}
					}
				}
			}
			else
			{
				final Object o = index.mySymbols.get(requiredNameId);

				if(o instanceof Object[])
				{
					for(Object o2 : (Object[]) o)
					{
						final JSNamespace o2Ns = index.mySymbolNameComponents.get(o2);
						if(o2Ns == jsNamespace)
						{
							if(!dispatchProcessorCall(index, (JSNamedElementProxy) o2, myProcessor))
							{
								return false;
							}
						}
					}
				}
				else
				{
					final JSNamespace oNs = o != null ? index.mySymbolNameComponents.get(o) : null;
					if(oNs == jsNamespace)
					{
						if(!dispatchProcessorCall(index, (JSNamedElementProxy) o, myProcessor))
						{
							return false;
						}
					}
				}
			}

			return true;
		}
	}
}
