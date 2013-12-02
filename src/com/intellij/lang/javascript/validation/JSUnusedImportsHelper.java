package com.intellij.lang.javascript.validation;

import gnu.trove.THashSet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlText;

public class JSUnusedImportsHelper
{
	private final Collection<PsiElement> myElements;

	public static class Results
	{
		public final Collection<JSImportStatement> unusedImports;
		public final Collection<JSImportStatement> allImports;
		public final Map<JSReferenceExpression, String> fqnsToReplaceWithImport;

		public Results(Map<JSReferenceExpression, String> fqnsToReplaceWithImport, Collection<JSImportStatement> unusedImports,
				Collection<JSImportStatement> allImports)
		{
			this.fqnsToReplaceWithImport = fqnsToReplaceWithImport;
			this.unusedImports = unusedImports;
			this.allImports = allImports;
		}
	}

	private static final Collection<? extends Class<? extends JSQualifiedNamedElement>> REFERENCED_ELEMENTS_CLASSES = Arrays.asList(JSClass.class,
			JSNamespaceDeclaration.class, JSFunction.class, JSVariable.class);

	// in Flex 3 for some reason fqn reference to function or variable does not reqiure explicit import statement
	private static final Collection<? extends Class<? extends JSQualifiedNamedElement>> QUALIFIED_REFERENCE_NEEDS_IMPORT = Arrays.asList(JSClass.class,
			JSNamespaceDeclaration.class);

	private static final Key<CachedValue<Results>> ourUnusedImportsKey = Key.create("js.unused.imports");

	private final THashSet<JSImportStatement> unusedImports = new THashSet<JSImportStatement>();
	private final Collection<JSImportStatement> allImports = new THashSet<JSImportStatement>();
	private final THashSet<JSImportStatement> importsUsedAheadOfDefinition = new THashSet<JSImportStatement>();
	private final Map<JSReferenceExpression, String> fqnsToReplaceWithImport = new HashMap<JSReferenceExpression, String>();
	private final PsiFile myContainingFile;

	private JSUnusedImportsHelper(PsiFile containingFile, Collection<PsiElement> elements)
	{
		myContainingFile = containingFile;
		myElements = elements;
	}

	private void registerUnused(final JSImportStatement importStatement)
	{
		allImports.add(importStatement);

		final String importText = importStatement.getImportText();
		if(importText == null)
		{
			return;
		}

		if(importsUsedAheadOfDefinition.contains(importStatement))
		{
			return;
		}

		unusedImports.add(importStatement);
	}

	private static boolean isInstance(PsiElement element, Collection<? extends Class<?>> classes)
	{
		for(Class<?> clazz : classes)
		{
			if(clazz.isInstance(element))
			{
				return true;
			}
		}
		return false;
	}

	private void process(JSReferenceExpression node)
	{
		if(node.getQualifier() == null)
		{
			String thisPackage = JSResolveUtil.findPackageStatementQualifier(node);
			registerUsedImportsFromResolveResults(node, thisPackage);
		}
		else
		{
			registerUsedImportsFromResolveResults(node, null);

			Pair<Boolean, Boolean> replaceStatus = getReplaceStatus(node);

			if(replaceStatus.second)
			{
				if(sameContainingFile(node.getContainingFile(), myContainingFile))
				{
					fqnsToReplaceWithImport.put(node, node.getText());
				}
			}
		}
	}

	private void registerUsedImportsFromResolveResults(JSReferenceExpression node, String thisPackage)
	{
		for(ResolveResult r : node.multiResolve(false))
		{
			// TODO can we get different import statements here?
			if(r instanceof JSResolveUtil.MyResolveResult)
			{
				JSImportStatement importStatement = ((JSResolveUtil.MyResolveResult) r).getImportUsed();

				if(importStatement != null && isInstance(r.getElement(), REFERENCED_ELEMENTS_CLASSES))
				{
					String importString = importStatement.getImportText();
					String importedPackage = importString.substring(0, importString.lastIndexOf('.'));
					if(thisPackage == null || !thisPackage.equals(importedPackage))
					{
						registerUsed(importStatement);
					}
				}
			}
		}
	}

	private static boolean sameContainingFile(PsiFile containingFile, PsiFile subjectFile)
	{
		return subjectFile instanceof XmlFile ? getContainingFile(containingFile) == subjectFile : containingFile == subjectFile;
	}

	public static boolean isSomeNodeThatShouldNotHaveImportsWhenQualified(JSReferenceExpression expression, PsiElement element)
	{
		Pair<Boolean, Boolean> pair = getReplaceStatus(expression, element);
		return !pair.first;
	}

	private static Pair<Boolean, Boolean> getReplaceStatus(JSReferenceExpression node)
	{
		return getReplaceStatus(node, null);
	}

	/**
	 * @return Pair(needs import statement, can be replaced)
	 */
	private static Pair<Boolean, Boolean> getReplaceStatus(JSReferenceExpression node, PsiElement resolve)
	{
		if(resolve == null)
		{
			resolve = JSResolveUtil.findClassByQName(node.getText(), node);
		}
		PsiElement parent = node.getParent();
		if(parent instanceof JSImportStatement)
		{
			return Pair.create(false, false);
		}

		if(parent instanceof JSExpressionStatement && !(resolve instanceof JSNamespaceDeclaration))
		{
			return Pair.create(false, false);
		}

		// implicit parameter of anonymous event listener
		if(parent instanceof JSParameter)
		{
			JSFunction parentFunctionExpression = PsiTreeUtil.getParentOfType(node, JSFunction.class);
			if(parentFunctionExpression instanceof JSFunctionExpression)
			{
				if(node.getContainingFile().getContext() instanceof XmlAttributeValue || node.getContainingFile().getContext() instanceof XmlText)
				{
					return Pair.create(false, false);
				}
			}
		}

		if(parent.getNode().getElementType() == JSElementTypes.EXTENDS_LIST)
		{
			if(parent.getParent() instanceof JSClass)
			{
				String className = ((JSClass) parent.getParent()).getName();
				if(StringUtil.isNotEmpty(className) && className.equals(node.getReferencedName()))
				{
					return Pair.create(true, false);
				}
			}
		}

		// part of implements list of mxml component
		if(parent.getNode().getElementType() == JSElementTypes.IMPLEMENTS_LIST)
		{
			if(node.getContainingFile().getContext() instanceof XmlAttributeValue)
			{
				return Pair.create(false, false);
			}
		}

		PsiElement element = resolve;
		if(element == null || !isInstance(element, REFERENCED_ELEMENTS_CLASSES))
		{
			return Pair.create(false, false);
		}

		return Pair.create(isInstance(element, QUALIFIED_REFERENCE_NEEDS_IMPORT), true);
	}

	private void registerUsed(JSImportStatement importStatement)
	{
		allImports.add(importStatement);
		if(importStatement.getImportText() == null)
		{
			return;
		}

		if(!unusedImports.remove(importStatement))
		{
			importsUsedAheadOfDefinition.add(importStatement);
		}
	}

	private Collection<JSImportStatement> filter(Collection<JSImportStatement> original)
	{
		Collection<JSImportStatement> result = new ArrayList<JSImportStatement>();
		for(JSImportStatement importStatement : original)
		{
			if(importStatement.isValid() && sameContainingFile(importStatement.getContainingFile(), myContainingFile))
			{
				result.add(importStatement);
			}
		}
		return result;
	}


	public static Results getUnusedImports(PsiFile file)
	{
		PsiFile containingFile = getContainingFile(file);

		CachedValue<Results> data = containingFile.getUserData(ourUnusedImportsKey);
		if(data == null)
		{
			final PsiFile containingFile1 = containingFile;
			data = CachedValuesManager.getManager(file.getProject()).createCachedValue(new CachedValueProvider<Results>()
			{
				public Result<Results> compute()
				{
					final Map<XmlTag, Collection<PsiElement>> allElements = new HashMap<XmlTag, Collection<PsiElement>>();
					Collection<JSFile> processedFiles = new THashSet<JSFile>();
					collectElements(null, containingFile1, allElements, processedFiles);

					Results allResults = new Results(new HashMap<JSReferenceExpression, String>(), new ArrayList<JSImportStatement>(),
							new ArrayList<JSImportStatement>());
					for(Collection<PsiElement> elements : allElements.values())
					{
						Results results = new JSUnusedImportsHelper(containingFile1, elements).getUnusedImports();
						allResults.fqnsToReplaceWithImport.putAll(results.fqnsToReplaceWithImport);
						allResults.unusedImports.addAll(results.unusedImports);
						allResults.allImports.addAll(results.allImports);
					}

					// TODO explicit depencencies
					return new Result<Results>(allResults, PsiModificationTracker.MODIFICATION_COUNT);
				}
			}, false);
			containingFile1.putUserData(ourUnusedImportsKey, data);
		}
		return data.getValue();
	}

	private static PsiFile getContainingFile(PsiFile file)
	{
		return file.getContext() != null ? file.getContext().getContainingFile() : file;
	}

	private Results getUnusedImports()
	{
		for(PsiElement e : myElements)
		{
			if(e instanceof JSImportStatement)
			{
				JSImportStatement importStatement = (JSImportStatement) e;
				registerUnused(importStatement);
			}
			else if(e instanceof JSReferenceExpression)
			{
				process((JSReferenceExpression) e);
			}
		}

		return new Results(fqnsToReplaceWithImport, filter(unusedImports), filter(allImports));
	}

	private static void collectElements(@Nullable final XmlTag rootTag, final PsiFile file, final Map<XmlTag, Collection<PsiElement>> result,
			final Collection<JSFile> processedFiles)
	{
		if(processedFiles.contains(file))
		{
			return;
		}
		if(file instanceof JSFile)
		{
			processedFiles.add((JSFile) file);

			PsiTreeUtil.processElements(file, new PsiElementProcessor()
			{
				public boolean execute(PsiElement element)
				{
					if(element instanceof JSIncludeDirective)
					{
						PsiFile includedFile = ((JSIncludeDirective) element).resolveFile();
						// we check processed files before since we may include this file to self and setting context will make cycle
						if(includedFile instanceof JSFile && !processedFiles.contains((JSFile) includedFile))
						{
							includedFile.putUserData(JSResolveUtil.contextKey, element);
							collectElements(rootTag, includedFile, result, processedFiles);
						}
					}
					else if(element instanceof JSElement && !(element instanceof JSFile))
					{
						Collection<PsiElement> elements = result.get(rootTag);
						if(elements == null)
						{
							elements = new ArrayList<PsiElement>();
							result.put(rootTag, elements);
						}
						elements.add(element);
					}
					return true;
				}
			});
		}
		else if(JavaScriptSupportLoader.isFlexMxmFile(file))
		{
			XmlBackedJSClassImpl.visitInjectedFiles((XmlFile) file, new XmlBackedJSClassImpl.InjectedFileVisitor()
			{
				public void visit(XmlTag rootTag, JSFile jsFile)
				{
					collectElements(rootTag, jsFile, result, processedFiles);
				}
			});
		}
	}

}
