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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.xml.XmlElementDescriptor;
import com.intellij.xml.XmlNSDescriptor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.language.Language;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.LightElement;
import consulo.language.psi.*;
import consulo.language.psi.meta.PsiMetaData;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.util.IncorrectOperationException;
import consulo.xml.psi.xml.*;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @since Maxim.Mossienko
 */
public class JSXmlLiteralExpressionImpl extends JSExpressionImpl implements JSLiteralExpression, XmlTag
{
	private static class LightXmlValue extends LightElement implements XmlTagValue
	{
		protected LightXmlValue(PsiManager manager, Language language)
		{
			super(manager, language);
		}

		@Override
		public TextRange getTextRange()
		{
			return TextRange.EMPTY_RANGE;
		}

		@Nonnull
		@Override
		public XmlTagChild[] getChildren()
		{
			return XmlTagChild.EMPTY_ARRAY;
		}

		@Override
		public String toString()
		{
			return null;
		}

		@Override
		public int getTextOffset()
		{
			return 0;
		}

		@Nonnull
		@Override
		public XmlText[] getTextElements()
		{
			return new XmlText[0];
		}

		@Nonnull
		@Override
		public String getTrimmedText()
		{
			return "";
		}

		@Override
		public void setText(String s)
		{

		}

		@Override
		public void setEscapedText(String s)
		{

		}

		@Override
		public boolean hasCDATA()
		{
			return false;
		}
	}

	public JSXmlLiteralExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Nonnull
	@Override
	public PsiReference[] getReferences()
	{
		return ReferenceProvidersRegistry.getReferencesFromProviders(this);
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public PsiElement[] getChildren()
	{
		PsiElement psiChild = getFirstChild();
		if(psiChild == null)
		{
			return PsiElement.EMPTY_ARRAY;
		}

		List<PsiElement> result = new ArrayList<>();
		while(psiChild != null)
		{
			result.add(psiChild);

			psiChild = psiChild.getNextSibling();
		}
		return PsiUtilCore.toPsiElementArray(result);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
	}

	@Override
	@RequiredReadAction
	public String getName()
	{
		PsiElement childByType = findChildByType(JSTokenTypes.XML_NAME);
		return childByType != null ? childByType.getText() : "";
	}

	@RequiredWriteAction
	@Override
	public PsiElement setName(@Nonnull @NonNls String s) throws IncorrectOperationException
	{
		return null;
	}

	@Nonnull
	@Override
	public String getNamespace()
	{
		return "";
	}

	@Nonnull
	@Override
	public String getLocalName()
	{
		return getName();
	}

	@Nullable
	@Override
	public XmlElementDescriptor getDescriptor()
	{
		return null;
	}

	@Nonnull
	@Override
	public XmlAttribute[] getAttributes()
	{
		return new XmlAttribute[0];
	}

	@Nullable
	@Override
	public XmlAttribute getAttribute(@NonNls String name, @NonNls String namespace)
	{
		return null;
	}

	@Nullable
	@Override
	public XmlAttribute getAttribute(@NonNls String qname)
	{
		return null;
	}

	@Nullable
	@Override
	public String getAttributeValue(@NonNls String name, @NonNls String namespace)
	{
		return null;
	}

	@Nullable
	@Override
	public String getAttributeValue(@NonNls String qname)
	{
		return null;
	}

	@Override
	public XmlAttribute setAttribute(@NonNls String name, @NonNls String namespace, @NonNls String value) throws IncorrectOperationException
	{
		return null;
	}

	@Override
	public XmlAttribute setAttribute(@NonNls String qname, @NonNls String value) throws IncorrectOperationException
	{
		return null;
	}

	@Override
	public XmlTag createChildTag(@NonNls String localName, @NonNls String namespace, @Nullable @NonNls String bodyText, boolean enforceNamespacesDeep)
	{
		return null;
	}

	@Override
	public XmlTag addSubTag(XmlTag subTag, boolean first)
	{
		return null;
	}

	@Nonnull
	@Override
	public XmlTag[] getSubTags()
	{
		return new XmlTag[0];
	}

	@Nonnull
	@Override
	public XmlTag[] findSubTags(@NonNls String qname)
	{
		return new XmlTag[0];
	}

	@Nonnull
	@Override
	public XmlTag[] findSubTags(@NonNls String localName, @Nullable String namespace)
	{
		return new XmlTag[0];
	}

	@Nullable
	@Override
	public XmlTag findFirstSubTag(@NonNls String qname)
	{
		return null;
	}

	@Nonnull
	@Override
	public String getNamespacePrefix()
	{
		return "";
	}

	@Nonnull
	@Override
	public String getNamespaceByPrefix(@NonNls String prefix)
	{
		return "";
	}

	@Nullable
	@Override
	public String getPrefixByNamespace(@NonNls String namespace)
	{
		return null;
	}

	@Override
	public String[] knownNamespaces()
	{
		return new String[0];
	}

	@Override
	public boolean hasNamespaceDeclarations()
	{
		return false;
	}

	@Nonnull
	@Override
	public Map<String, String> getLocalNamespaceDeclarations()
	{
		return Collections.emptyMap();
	}

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		return 0;
	}

	@Nonnull
	@Override
	public XmlTagValue getValue()
	{
		return new LightXmlValue(getManager(), getLanguage());
	}

	@Nullable
	@Override
	public XmlNSDescriptor getNSDescriptor(@NonNls String namespace, boolean strict)
	{
		return null;
	}

	@Override
	public boolean isEmpty()
	{
		return true;
	}

	@Override
	public void collapseIfEmpty()
	{

	}

	@Nullable
	@Override
	public String getSubTagText(@NonNls String qname)
	{
		return null;
	}

	@Nullable
	@Override
	public PsiMetaData getMetaData()
	{
		return null;
	}

	@Override
	public XmlTag getParentTag()
	{
		return null;
	}

	@Nullable
	@Override
	public XmlTagChild getNextSiblingInTag()
	{
		return null;
	}

	@Nullable
	@Override
	public XmlTagChild getPrevSiblingInTag()
	{
		return null;
	}

	@Override
	public boolean processElements(PsiElementProcessor processor, PsiElement place)
	{
		return false;
	}
}