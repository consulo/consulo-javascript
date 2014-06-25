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

package com.intellij.lang.javascript.flex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.idea.LoggerFactory;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlText;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;

/**
 * @author Maxim.Mossienko
 *         Date: May 3, 2008
 *         Time: 8:35:33 PM
 */
public class ImportUtils
{
	@NonNls
	private static final String CDATA = "<![CDATA[";
	@NonNls
	private static final String SCRIPT_TAG_NAME = "Script";

	private static final List<Class<? extends JSElement>> ANONYMOUS_EVENT_LISTENER_CLASSES = Arrays.asList(JSBlockStatement.class,
			JSFunctionExpression.class, JSParenthesizedExpression.class, JSCallExpression.class, JSExpressionStatement.class, JSFile.class);

	@Nullable
	private static PsiElement findLBrace(final JSElement holder)
	{
		for(PsiElement child = holder.getFirstChild(); child != null; child = child.getNextSibling())
		{
			if(child.getNode().getElementType() == JSTokenTypes.LBRACE)
			{
				return child;
			}
		}
		return null;
	}

	private static PsiElement specifyInsertionPlace(PsiElement insertBefore, String fqn)
	{
		JSImportStatement earlyImport = findEarlyImport(insertBefore);
		if(earlyImport == null)
		{
			return insertBefore;
		}

		while(compareImports(fqn, earlyImport.getImportText()) > 0)
		{
			if(earlyImport.getNextSibling() instanceof JSImportStatement)
			{
				earlyImport = (JSImportStatement) earlyImport.getNextSibling();
			}
			else if(earlyImport.getNextSibling() instanceof PsiWhiteSpace && earlyImport.getNextSibling().getNextSibling() instanceof JSImportStatement)
			{
				earlyImport = (JSImportStatement) earlyImport.getNextSibling().getNextSibling();
			}
			else
			{
				return earlyImport.getNextSibling();
			}
		}
		return earlyImport;
	}

	public static void doImport(@NotNull PsiElement subject, final @NotNull String fqn)
	{
		assert fqn.contains(".") : "Qualified name belongs to default package: " + fqn;

		if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(subject.getContainingFile()))
		{
			return;
		}
		Project project = subject.getProject();

		try
		{
			final JSReferenceExpression refExpr = PsiTreeUtil.getNonStrictParentOfType(subject, JSReferenceExpression.class);
			if(refExpr != null && JSResolveUtil.referenceExpressionShouldBeQualified(refExpr))
			{
				refExpr.replace(JSChangeUtil.createExpressionFromText(project, fqn).getPsi()); // TODO should commit corresponding document before?
				return;
			}

			JSElement importHolder = getImportHolder(subject, JSPackageStatement.class, JSFile.class);
			if(importHolder == null)
			{ // importHolder is null when completing js2 class name from js code
				return;
			}

			PsiFile file = importHolder.getContainingFile();
			Document document = PsiDocumentManager.getInstance(project).getDocument(file);
			PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document);

			String textToInsert = createImportBlock(project, Collections.singletonList(fqn));

			Pair<PsiElement, Boolean/*before*/> insertionPlace = getImportInsertionPlace(importHolder);
			if(!insertionPlace.second && insertionPlace.first.getNextSibling() != null)
			{
				insertionPlace = Pair.create(insertionPlace.first.getNextSibling(), true);
			}

			final int offset;
			final String prefix;
			final String suffix;
			if(insertionPlace.second)
			{
				PsiElement insertBefore = specifyInsertionPlace(insertionPlace.first, fqn);
				offset = insertBefore.getTextRange().getStartOffset();
				prefix = (insertBefore.getPrevSibling() == null && file.getContext() == null) || insertBefore.getPrevSibling() instanceof PsiWhiteSpace ? "" :
						"\n";
				suffix = insertBefore instanceof PsiWhiteSpace ? "" : " ";
			}
			else
			{
				offset = insertionPlace.first.getTextRange().getEndOffset();
				prefix = insertionPlace.first instanceof PsiWhiteSpace ? "" : "\n";
				suffix = "";
			}

			document.insertString(offset, prefix + textToInsert + suffix);
			PsiDocumentManager.getInstance(project).commitDocument(document);

			PsiElement inserted = file.findElementAt(offset);
			if(prefix.length() > 0)
			{
				if(inserted.getNextSibling() instanceof JSImportStatement)
				{
					inserted = inserted.getNextSibling();
				}
			}
			else
			{
				JSImportStatement importStatement = PsiTreeUtil.getParentOfType(inserted, JSImportStatement.class);
				if(importStatement != null)
				{
					inserted = importStatement;
				}
			}
			PsiElement formatFrom = inserted.getPrevSibling() instanceof PsiWhiteSpace ? inserted.getPrevSibling() : inserted;
			PsiElement formatTo = inserted.getNextSibling() instanceof PsiWhiteSpace ? inserted.getNextSibling() : inserted;

			PsiFile realFile = file.getContext() != null ? file.getContext().getContainingFile() : file;
			final TextRange injectionOffset = InjectedLanguageManager.getInstance(project).injectedToHost(inserted, inserted.getTextRange());

			CodeStyleManager.getInstance(project).reformatText(realFile, injectionOffset.getStartOffset() + formatFrom.getTextRange().getStartOffset(),
					injectionOffset.getEndOffset() + formatTo.getTextRange().getEndOffset());
		}
		catch(IncorrectOperationException ex)
		{
			LoggerFactory.getInstance().getLoggerInstance(ImportUtils.class.getName()).error(ex);
		}
	}

	private static JSElement getAnonymousEventHandlerBody(JSFile injectedFile)
	{
		// TODO more elegant way?!
		if(injectedFile.getFirstChild() instanceof JSExpressionStatement)
		{
			JSExpressionStatement expressionStatement = (JSExpressionStatement) injectedFile.getFirstChild();
			if(expressionStatement.getExpression() instanceof JSCallExpression)
			{
				JSCallExpression callExpression = (JSCallExpression) expressionStatement.getExpression();
				if(callExpression.getMethodExpression() instanceof JSParenthesizedExpression)
				{
					JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) callExpression.getMethodExpression();
					if(parenthesizedExpression.getInnerExpression() instanceof JSFunctionExpression)
					{
						JSFunctionExpression functionExpression = (JSFunctionExpression) parenthesizedExpression.getInnerExpression();
						JSFunction function = functionExpression.getFunction();
						if(function.getBody().length > 0)
						{
							return function.getBody()[0];
						}
					}
				}
			}
		}
		assert false : "Couldn't find anonymous event handler body: " + injectedFile;
		return null;
	}

	private static boolean isAnonymousEventHandlerTag(JSFile jsFile)
	{
		PsiElement context = jsFile.getContext();
		return context instanceof XmlText && !SCRIPT_TAG_NAME.equals(((XmlTag) context.getParent()).getLocalName());
	}

	private static boolean isAnonymousEventHandlerAttribute(JSFile jsFile)
	{
		PsiElement context = jsFile.getContext();
		return context instanceof XmlAttributeValue;
	}

	public static boolean isAnonymousEventHandler(JSBlockStatement block)
	{
		JSFile file = (JSFile) iterateUp(block, ANONYMOUS_EVENT_LISTENER_CLASSES);
		return file != null && (isAnonymousEventHandlerTag(file) || isAnonymousEventHandlerAttribute(file));
	}

	@Nullable
	private static PsiElement iterateUp(PsiElement element, List<Class<? extends JSElement>> classes)
	{
		Iterator<Class<? extends JSElement>> i = classes.iterator();
		while(i.hasNext())
		{
			if(element == null || !i.next().isInstance(element))
			{
				return null;
			}
			if(i.hasNext())
			{
				element = element.getParent();
			}
		}
		return element;
	}

	@Nullable
	private static JSElement getImportHolderFromXmlBackedClass(final XmlBackedJSClassImpl jsClass)
	{
		try
		{
			return jsClass.createOrGetFirstScriptTag();
		}
		catch(IncorrectOperationException ex)
		{
			Logger.getInstance(ImportUtils.class.getName()).error(ex);
		}
		return null;
	}


	private static int compareImports(String qname1, String qname2)
	{
		// TODO keep certain classes at the top
		return Comparing.compare(qname1, qname2);
	}

	public static Pair<PsiElement, Boolean /*before*/> getImportInsertionPlace(JSElement holder)
	{
		PsiElement insertionPlace;
		final boolean before;
		if(holder instanceof JSPackageStatement)
		{
			insertionPlace = findLBrace(holder);
			assert insertionPlace != null : "LBrace not found";
			before = false;
		}
		else if(holder instanceof JSFunction)
		{
			final JSBlockStatement block = PsiTreeUtil.getChildOfType(holder, JSBlockStatement.class);
			assert block != null : "Function block not found";
			insertionPlace = findLBrace(block);
			before = false;
		}
		else
		{ //JSFile
			if(isAnonymousEventHandlerTag((JSFile) holder))
			{
				holder = getAnonymousEventHandlerBody((JSFile) holder);
				insertionPlace = findLBrace(holder);
				if(hasCDATA(insertionPlace.getNextSibling()))
				{
					insertionPlace = insertionPlace.getNextSibling();
				}
				before = false;
			}
			else
			{
				JSPackageStatement aPackage = PsiTreeUtil.getChildOfType(holder, JSPackageStatement.class);
				if(aPackage != null)
				{
					insertionPlace = aPackage;
					before = false;
				}
				else
				{
					insertionPlace = holder.getFirstChild();
					before = !hasCDATA(insertionPlace);
				}
			}
		}
		return Pair.create(insertionPlace, before);
	}

	private static boolean hasCDATA(@Nullable PsiElement element)
	{
		return element instanceof PsiWhiteSpace && element.getText().contains(CDATA);
	}

	@Nullable
	public static JSElement getImportHolder(PsiElement origin, Class<? extends JSElement>... classes)
	{
		if(origin instanceof XmlBackedJSClassImpl)
		{
			return getImportHolderFromXmlBackedClass((XmlBackedJSClassImpl) origin);
		}

		JSElement importHolder = PsiTreeUtil.getParentOfType(origin, classes);
		if(importHolder instanceof JSFunctionExpression && (isAnonymousEventHandlerTag((JSFile) importHolder.getContainingFile()) ||
				isAnonymousEventHandlerAttribute((JSFile) importHolder.getContainingFile())))
		{
			importHolder = ArrayUtil.contains(JSFile.class, classes) ? (JSElement) importHolder.getContainingFile() : null;
		}

		if(importHolder instanceof JSFile)
		{
			if(isAnonymousEventHandlerAttribute((JSFile) importHolder))
			{
				XmlBackedJSClassImpl jsClass = JSResolveUtil.getXmlBackedClass((JSFile) importHolder);
				assert jsClass != null;
				importHolder = getImportHolderFromXmlBackedClass(jsClass);
			}
		}
		return importHolder;
	}

	public static String createImportBlock(Project project, Collection<String> fqns)
	{
		List<String> sorted = new ArrayList<String>(fqns);
		Collections.sort(sorted, new Comparator<String>()
		{
			@Override
			public int compare(final String o1, final String o2)
			{
				return compareImports(o1, o2);
			}
		});

		final String semicolon = JSChangeUtil.getSemicolon(project);
		StringBuilder s = new StringBuilder();
		for(String fqn : sorted)
		{
			s.append("import ").append(fqn).append(semicolon);
		}
		return s.toString();
	}

	@Nullable
	public static JSImportStatement findEarlyImport(@Nullable PsiElement startFrom)
	{
		for(PsiElement element = startFrom; element != null; element = element.getNextSibling())
		{
			if(element instanceof JSImportStatement)
			{
				return (JSImportStatement) element;
			}
			if(element instanceof JSClass || element instanceof JSStatement || element instanceof JSFunction)
			{
				break;
			}
		}
		return null;
	}
}
