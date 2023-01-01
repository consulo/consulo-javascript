/*
 * Copyright 2013-2015 must-be.org
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

package consulo.json.validation.completion;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.AllIcons;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.json.validation.JsonFileDescriptorProviders;
import consulo.json.validation.NativeArray;
import consulo.json.validation.descriptor.JsonObjectDescriptor;
import consulo.json.validation.descriptor.JsonPropertyDescriptor;
import consulo.json.validation.inspections.PropertyValidationInspection;
import consulo.language.Language;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.completion.*;
import consulo.language.editor.completion.lookup.InsertHandler;
import consulo.language.editor.completion.lookup.InsertionContext;
import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.editor.completion.lookup.LookupElementBuilder;
import consulo.language.pattern.StandardPatterns;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.ProcessingContext;
import consulo.ui.image.Image;
import consulo.util.collection.ContainerUtil;
import consulo.util.lang.StringUtil;

import javax.annotation.Nonnull;
import java.util.*;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
@ExtensionImpl
public class JsonCompletionContributor extends CompletionContributor
{
	public JsonCompletionContributor()
	{
		extend(CompletionType.BASIC, StandardPatterns.psiElement(JSTokenTypes.IDENTIFIER), new CompletionProvider()
		{
			@RequiredReadAction
			@Override
			public void addCompletions(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result)
			{
				addVariants(parameters, result, true);
			}
		});

		extend(CompletionType.BASIC, StandardPatterns.psiElement(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL), new CompletionProvider()
		{
			@RequiredReadAction
			@Override
			public void addCompletions(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result)
			{
				PsiElement originalPosition = parameters.getOriginalPosition();
				if(originalPosition == null)
				{
					return;
				}
				if(!"\'\'".equals(originalPosition.getText()))
				{
					return;
				}
				addVariants(parameters, result, false);
			}
		});

		extend(CompletionType.BASIC, StandardPatterns.psiElement(JSTokenTypes.STRING_LITERAL), new CompletionProvider()
		{
			@RequiredReadAction
			@Override
			public void addCompletions(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result)
			{
				PsiElement originalPosition = parameters.getOriginalPosition();
				if(originalPosition == null)
				{
					return;
				}
				if(!"\"\"".equals(originalPosition.getText()))
				{
					return;
				}
				addVariants(parameters, result, false);
			}
		});
	}

	@RequiredReadAction
	private static void addVariants(@Nonnull CompletionParameters parameters, @Nonnull CompletionResultSet result, boolean quotes)
	{
		final PsiFile originalFile = parameters.getOriginalFile();

		JsonObjectDescriptor rootDescriptor = JsonFileDescriptorProviders.getRootDescriptor(originalFile);
		if(rootDescriptor == null)
		{
			return;
		}

		PsiElement position = parameters.getPosition();
		final PsiElement jsProperty = position.getParent();
		if(!(jsProperty instanceof JSProperty))
		{
			return;
		}

		Collection<JSProperty> jsProperties = PropertyValidationInspection.buildPropertiesAsTree(position, rootDescriptor);
		if(jsProperties.isEmpty())
		{
			return;
		}

		JsonObjectDescriptor parentObject = null;
		JsonPropertyDescriptor currentProperty = null;
		JsonObjectDescriptor currentObject = rootDescriptor;
		for(JSProperty property : jsProperties)
		{
			String name = property.getName();
			if(name == null)
			{
				return;
			}

			currentProperty = currentObject.getProperty(name);
			if(currentProperty == null)
			{
				if(property == jsProperty)
				{
					parentObject = currentObject;
					break;
				}
				else
				{
					return;
				}
			}
			else if(currentProperty.getValue() instanceof JsonObjectDescriptor)
			{
				currentObject = (JsonObjectDescriptor) currentProperty.getValue();
			}
			else if(currentProperty.getValue() instanceof NativeArray)
			{
				Object componentType = ((NativeArray) currentProperty.getValue()).getComponentType();
				if(componentType instanceof JsonObjectDescriptor)
				{
					currentObject = (JsonObjectDescriptor) componentType;
				}
				else
				{
					return;
				}
			}
			else
			{
				break;
			}
		}

		if(parentObject == null)
		{
			return;
		}

		Map<String, JsonPropertyDescriptor> properties = parentObject.getProperties();

		Set<String> alreadyDefined = Collections.emptySet();
		PsiElement parent = jsProperty.getParent();
		if(parent instanceof JSObjectLiteralExpression)
		{
			JSProperty[] parentProperties = ((JSObjectLiteralExpression) parent).getProperties();
			alreadyDefined = new HashSet<String>(parentProperties.length);
			for(JSProperty parentProperty : parentProperties)
			{
				ContainerUtil.addIfNotNull(alreadyDefined, parentProperty.getName());
			}
		}

		for(final Map.Entry<String, JsonPropertyDescriptor> entry : properties.entrySet())
		{
			String key = entry.getKey();
			if(key == null || alreadyDefined.contains(key))
			{
				continue;
			}
			LookupElementBuilder builder = LookupElementBuilder.create(quotes ? StringUtil.QUOTER.apply(key) : key);
			builder = builder.withPresentableText(quotes ? StringUtil.QUOTER.apply(key) : key);
			builder = builder.withIcon((Image) AllIcons.Nodes.Property);
			final JsonPropertyDescriptor value = entry.getValue();
			if(value.isDeprecated())
			{
				builder = builder.strikeout();
			}
			final Object type = value.getValue();
			builder = builder.withTypeText(formatValue(type), true);

			if(quotes && ((JSProperty) jsProperty).getValue() == null)
			{
				builder = builder.withInsertHandler(new InsertHandler<LookupElement>()
				{
					@Override
					public void handleInsert(InsertionContext context, LookupElement item)
					{
						if(type == Object.class)
						{
							context.getDocument().insertString(context.getTailOffset(), ": {\n}");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset() - 2);

							context.commitDocument();
							CodeStyleManager.getInstance(context.getProject()).reformatRange(originalFile, context.getStartOffset(), context.getTailOffset());
						}
						else if(type == Boolean.class)
						{
							context.getDocument().insertString(context.getTailOffset(), ": false");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset());
						}
						else if(type == String.class)
						{
							context.getDocument().insertString(context.getTailOffset(), ": \"\"");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset() - 1);
						}
						else if(type == Number.class)
						{
							context.getDocument().insertString(context.getTailOffset(), ": 0");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset());
						}
						else if(value.getValue() instanceof NativeArray)
						{
							context.getDocument().insertString(context.getTailOffset(), ": []");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset() - 1);
						}
						else
						{
							context.getDocument().insertString(context.getTailOffset(), ": ");
							context.getEditor().getCaretModel().moveToOffset(context.getTailOffset());
						}
					}
				});
			}
			result.addElement(builder);
		}
	}

	private static String formatValue(Object value)
	{
		if(value instanceof Class)
		{
			return StringUtil.decapitalize(((Class) value).getSimpleName());
		}
		else if(value instanceof NativeArray)
		{
			return formatValue(((NativeArray) value).getComponentType()) + "[]";
		}
		// JsonObjectDescriptor
		return "object";
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}
