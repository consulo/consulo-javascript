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

package com.intellij.lang.javascript;

import com.intellij.lang.javascript.flex.AnnotationBackedDescriptor;
import com.intellij.xml.XmlElementDescriptorWithCDataContent;
import consulo.component.util.pointer.NamedPointer;
import consulo.document.util.TextRange;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.LanguagePointerUtil;
import consulo.language.inject.MultiHostInjector;
import consulo.language.inject.MultiHostRegistrar;
import consulo.language.psi.OuterLanguageElement;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.meta.PsiMetaData;
import consulo.util.lang.StringUtil;
import consulo.xml.psi.html.HtmlTag;
import consulo.xml.psi.xml.*;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:40:38 PM
 */
public abstract class JSLanguageInjector implements MultiHostInjector
{
	@NonNls
	private static final String JAVASCRIPT_PREFIX = "javascript:";
	@NonNls
	public static final String JSP_URI = "http://java.sun.com/JSP/Page";

	private static final NamedPointer<Language> CSS_LANGUAGE = LanguagePointerUtil.createPointer("CSS");

	@Override
	public void injectLanguages(@Nonnull MultiHostRegistrar registrar, @Nonnull PsiElement host)
	{

		if(host instanceof XmlAttributeValue)
		{
			final PsiElement attribute = host.getParent();
			final PsiElement tag = attribute.getParent();

			if(attribute instanceof XmlAttribute && tag instanceof XmlTag)
			{
				if(host.getTextLength() == 0)
				{
					return;
				}
				@NonNls String attrName = ((XmlAttribute) attribute).getName();
				if(tag instanceof HtmlTag)
				{
					attrName = attrName.toLowerCase();
				}

				if("href".equals(attrName) && ((XmlAttributeValue) host).getValue().startsWith(JAVASCRIPT_PREFIX))
				{
					injectJSIntoAttributeValue(registrar, (XmlAttributeValue) host, true);
				}
				else if(attrName.startsWith("on"))
				{
					final String ns = ((XmlTag) tag).getNamespace();
					if(JavaScriptSupportLoader.BINDOWS_URI.equals(ns) ||
							JavaScriptSupportLoader.isBindowsFile(host) ||
							isMozillaXulOrXblNs(ns))
					{
						TextRange range = new TextRange(1, host.getTextLength() - 1);
						registrar.startInjecting(JavaScriptLanguage.INSTANCE).addPlace(null, null, (PsiLanguageInjectionHost) host,
								range).doneInjecting();
					}
					else
					{
						checkMxmlInjection(registrar, host, attribute, tag);
					}
				}
				else if("style".equals(attrName) && isMozillaXulOrXblNs(((XmlTag) tag).getNamespace()) && CSS_LANGUAGE.get() != null)
				{
					registrar.startInjecting(CSS_LANGUAGE.get()).addPlace("inline.style {", "}", (PsiLanguageInjectionHost) host, new TextRange(1,
							host.getTextLength() - 1)).doneInjecting();
				}
				else if("implements".equals(attrName) && JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile()))
				{
					TextRange range = new TextRange(1, host.getTextLength() - 1);
					registrar.startInjecting(JavaScriptSupportLoader.ECMA_SCRIPT_L4).addPlace("class Foo implements ", " {}", (PsiLanguageInjectionHost) host,
							range).doneInjecting();
				}
				else if(attrName.equalsIgnoreCase("dojoType"))
				{
					TextRange range = new TextRange(1, host.getTextLength() - 1);
					registrar.startInjecting(JavaScriptSupportLoader.ECMA_SCRIPT_L4).addPlace("var a:", null, (PsiLanguageInjectionHost) host,
							range).doneInjecting();
				}
				else if(attrName.equalsIgnoreCase("djConfig"))
				{
					TextRange range = new TextRange(1, host.getTextLength() - 1);
					registrar.startInjecting(JavaScriptLanguage.INSTANCE).addPlace("a = { ", " }", (PsiLanguageInjectionHost) host,
							range).doneInjecting();
				}
				else
				{
					checkMxmlInjection(registrar, host, attribute, tag);
				}
			}
		}
		else if(host instanceof XmlText)
		{
			final PsiElement _tag = host.getParent();
			if(_tag instanceof XmlTag)
			{
				final XmlTag tag = (XmlTag) _tag;

				final @NonNls String localName = tag.getLocalName();

				if("attribute".equals(localName) && JSP_URI.equals(tag.getNamespace()))
				{
					@NonNls final String name = tag.getAttributeValue("name");
					if(name != null && name.startsWith("on"))
					{
						Language language = JavaScriptLanguage.INSTANCE;
						injectToXmlText(registrar, host, language, null, null);
					}
				}
				else if("Style".equals(localName) && JavaScriptSupportLoader.isMxmlNs(tag.getNamespace()) && CSS_LANGUAGE.get() != null)
				{
					injectToXmlText(registrar, host, CSS_LANGUAGE.get(), null, null);
				}
				else if((("script".equals(localName) && ((tag.getNamespacePrefix().length() > 0 && doInjectTo(tag)) || isMozillaXulOrXblNs(tag.getNamespace()))
				) ||
						"Script".equals(localName) ||
						"Metadata".equals(localName)) && !(tag instanceof HtmlTag))
				{
					boolean inject = true;
					boolean ecma = JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile());
					if(ecma && tag.getAttributeValue("source") != null)
					{
						inject = false;
					}
					Language language = ecma ? JavaScriptSupportLoader.ECMA_SCRIPT_L4 : JavaScriptLanguage.INSTANCE;
					if(inject)
					{
						injectToXmlText(registrar, host, language, null, null);
					}
				}
				else if(!(tag instanceof HtmlTag) && JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile()) && tag.getSubTags().length == 0)
				{
					injectInMxmlFile(registrar, host, tag.getDescriptor(), tag);
				}
				else
				{
					boolean getter;
					boolean setter = false;
					boolean field = false;
					boolean body = false;
					boolean handler = false;
					boolean constructor = false;

					if(((getter = "getter".equals(localName)) ||
							(setter = "setter".equals(localName)) ||
							(field = "field".equals(localName)) ||
							(body = "body".equals(localName)) ||
							(handler = "handler".equals(localName)) ||
							//(property = ("property".equals(localName) && tag.getSubTags().length == 0)) ||
							(constructor = "constructor".equals(localName))) && isMozillaXulOrXblNs(tag.getNamespace()))
					{
						@NonNls String prefix = null;
						@NonNls String suffix = null;

						if(getter || setter)
						{
							final String name = getNameFromTag(tag.getParentTag(), "property");
							prefix = "function " + (getter ? "get " : "set ") + name + "(" + (setter ? "val" : "") + ") {";
							suffix = "}";
						}
						else if(body)
						{
							final XmlTag parentTag = tag.getParentTag();
							String name = getNameFromTag(parentTag, "method");
							String signature = "";
							for(XmlTag ptag : parentTag.findSubTags("parameter"))
							{
								if(signature.length() > 0)
								{
									signature += ", ";
								}
								signature += ptag.getAttributeValue("name");
							}
							prefix = "function " + name + "(" + signature + ") {";
							suffix = "}";
						}
						else if(field)
						{
							prefix = "var " + tag.getAttributeValue("name") + " = ";
						}
						else if(handler)
						{
							prefix = "function " + tag.getAttributeValue("event") + " {";
							suffix = "}";
						}
						else if(constructor)
						{
							final XmlTag parentTag = tag.getParentTag();
							final XmlTag grandParentTag = parentTag != null ? parentTag.getParentTag() : null;
							prefix = "function " + (grandParentTag != null ? grandParentTag.getAttributeValue("id") : null) + "() {";
							suffix = "}";
						}

						injectToXmlText(registrar, host, JavaScriptLanguage.INSTANCE, prefix, suffix);
					}
				}
			}
		}
	}

	private boolean doInjectTo(final XmlTag tag)
	{
		final XmlTagValue value = tag.getValue();
		final XmlTagChild[] tagChildren = value.getChildren();

		return tagChildren.length == 1 && (tagChildren[0].getNode().getElementType() == XmlElementType.XML_CDATA || !tagChildren[0].textContains('<'));
	}

	private static void checkMxmlInjection(final MultiHostRegistrar registrar, final PsiElement host, final PsiElement attribute, final PsiElement _tag)
	{
		final PsiFile containingFile = _tag.getContainingFile();
		if(JavaScriptSupportLoader.isFlexMxmFile(containingFile))
		{
			injectInMxmlFile(registrar, host, ((XmlAttribute) attribute).getDescriptor(), (XmlTag) _tag);
		}
	}

	private static void injectToXmlText(final MultiHostRegistrar registrar, final PsiElement host, final Language language, final String prefix,
			final String suffix)
	{
		TextRange range = new TextRange(0, host.getTextLength());
		registrar.startInjecting(language).addPlace(prefix, suffix, (PsiLanguageInjectionHost) host, range).doneInjecting();
	}

	private static String getNameFromTag(final XmlTag parentTag, final @NonNls String s)
	{
		return parentTag != null && s.equals(parentTag.getLocalName()) ? parentTag.getAttributeValue("name") : "";
	}

	public static boolean isMozillaXulOrXblNs(final @NonNls String ns)
	{
		return "http://www.mozilla.org/xbl".equals(ns) || "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul".equals(ns);
	}

	public static void injectJSIntoAttributeValue(final MultiHostRegistrar registrar, final XmlAttributeValue host, final boolean startsWithPrefix)
	{
		final PsiElement[] myChildren = host.getChildren();
		int valueIndex = myChildren.length - 2;
		int valueTokenNumber = 1;
		if(valueIndex < 0)
		{
			valueIndex = 0;
			valueTokenNumber = 0;
		}

		final PsiElement valueChild = myChildren[valueIndex];

		if(valueChild instanceof XmlToken && ((XmlToken) valueChild).getTokenType() == XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN)
		{
			boolean injectFromTheBeginning = valueIndex == valueTokenNumber;
			if(!injectFromTheBeginning)
			{
				// check if well formed el holder
				boolean wellFormed = true;
				boolean badlyFormed = false;

				for(int i = valueTokenNumber; i < valueIndex; ++i)
				{
					final PsiElement currentElement = myChildren[i];

					if(currentElement instanceof XmlToken && ((XmlToken) currentElement).getTokenType() == XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN)
					{
						// ok child
						continue;
					}
					else
					{

						if(currentElement instanceof OuterLanguageElement)
						{
							final String prevText = myChildren[i - 1].getText();
							final String nextText = myChildren[i + 1].getText();

							if((StringUtil.endsWithChar(prevText, '\"') && StringUtil.startsWithChar(nextText, '\"')) || (StringUtil.endsWithChar(prevText,
									'\'') && StringUtil.startsWithChar(nextText, '\'')))
							{
								// ok child
								continue;
							}
							else if((StringUtil.endsWithChar(prevText, '(') && StringUtil.startsWithChar(nextText, ')')) && currentElement.textContains('<'))
							{
								badlyFormed = true;
							}
							else if(currentElement.textContains('$'))
							{
								final String s = currentElement.getText();
								if(s.startsWith("${") && s.endsWith("}") && s.length() > 2)
								{
									continue;
								}
							}
							else if(currentElement.textContains('%'))
							{
								badlyFormed = true;
							}
						}

						wellFormed = false;
						break;
					}
				}

				if(badlyFormed)
				{
					return;
				}
				if(wellFormed)
				{
					injectFromTheBeginning = wellFormed;
				}
			}
			TextRange range = new TextRange(injectFromTheBeginning ? valueTokenNumber + (startsWithPrefix ? JAVASCRIPT_PREFIX.length() : 0) : valueChild
					.getStartOffsetInParent(), host.getTextLength() - valueTokenNumber);
			Language language = JavaScriptLanguage.INSTANCE;
			registrar.startInjecting(language).addPlace(null, null, (PsiLanguageInjectionHost) host, range).doneInjecting();

		}
	}

	private static void injectInMxmlFile(final MultiHostRegistrar registrar, final PsiElement host, final PsiMetaData descriptor, XmlTag tag)
	{
		int offset = host instanceof XmlText ? 0 : 1;

		if(descriptor instanceof AnnotationBackedDescriptor && ((XmlElementDescriptorWithCDataContent) descriptor).requiresCdataBracesInContext(tag))
		{
			final int length = host.getTextLength();
			if(length < 2 * offset)
			{
				return;
			}
			String type = ((AnnotationBackedDescriptor) descriptor).getType();
			if(type == null)
			{
				type = "*";
			}
			final @NonNls String prefix = "(function (event:" + type + ") {";
			final @NonNls String suffix = "})();";

			if(host instanceof XmlText)
			{
				injectToXmlText(registrar, host, JavaScriptSupportLoader.ECMA_SCRIPT_L4, prefix, suffix);
			}
			else
			{
				TextRange range = new TextRange(offset, length - offset);

				registrar.startInjecting(JavaScriptSupportLoader.ECMA_SCRIPT_L4).addPlace(prefix, (host instanceof XmlAttributeValue ? "\n" : "") + suffix,
						(PsiLanguageInjectionHost) host, range).doneInjecting();
			}
		}
		else
		{
			final String text = StringUtil.stripQuotesAroundValue(host.getText());
			int openedBraces = 0;
			int start = -1;
			boolean addedSomething = false;
			boolean quoted = false;

			for(int i = 0; i < text.length(); ++i)
			{
				final char ch = text.charAt(i);

				if(quoted)
				{
					quoted = false;
					continue;
				}
				if(ch == '\\')
				{
					quoted = true;
				}
				else if(ch == '{')
				{
					if(openedBraces == 0)
					{
						start = i + 1;
					}
					openedBraces++;
				}
				else if(ch == '}')
				{
					openedBraces--;
					if(openedBraces == 0 && start != -1)
					{
						registrar.startInjecting(JavaScriptSupportLoader.ECMA_SCRIPT_L4).addPlace("(function (){}) (", "\n);", (PsiLanguageInjectionHost) host, new TextRange(offset + start, i + offset)).doneInjecting();
						addedSomething = true;
						start = -1;
					}
				}
			}

			if(!addedSomething)
			{
				final String trimmedText = text.trim();
				start = trimmedText.indexOf("@Embed");
				if(start == 0)
				{
					offset += text.indexOf(trimmedText);
					registrar.startInjecting(JavaScriptSupportLoader.ECMA_SCRIPT_L4).addPlace(null, null, (PsiLanguageInjectionHost) host, new TextRange(offset, trimmedText.length() + offset)).doneInjecting();
				}
			}
		}
	}
}
