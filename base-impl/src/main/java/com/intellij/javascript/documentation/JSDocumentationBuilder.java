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

/*
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Nov 15, 2006
 * Time: 4:48:04 PM
 */
package com.intellij.javascript.documentation;

import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.psi.*;
import consulo.language.editor.CodeInsightBundle;
import consulo.language.psi.PsiElement;
import consulo.util.io.URLUtil;
import consulo.util.lang.StringUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class JSDocumentationBuilder implements JSDocumentationProcessor
{
	private static class SymbolInfo
	{
		String visibility;
		StringBuilder description = new StringBuilder();
		String type;
		String access;
		boolean deprecated;
		String namespace;
	}

	private static class MethodInfo extends SymbolInfo
	{
		String methodType;
		Map<String, ParameterInfo> parameterInfoMap = new LinkedHashMap<String, ParameterInfo>();
		int parameterCount = -1;
		SymbolInfo returnInfo = new SymbolInfo();
	}

	private static class ParameterInfo extends SymbolInfo
	{
		boolean optional;
		String initialValue;
	}

	private boolean myOptionalParametersStarted;
	private boolean myEventsStarted;
	private SymbolInfo generationInfo;
	private ParameterInfo currentParameterInfo;
	private
	@NonNls
	StringBuilder result;

	private JSFunction function;
	private JSNamedElement namedItem;
	private final PsiElement myElement;
	private PsiElement contextElement;

	private int myNewLinesPendingCount;
	private boolean seenPre;
	private boolean seenSeeAlso;
	@NonNls
	private static final String BR_DELIMITER = "<BR>\n";

	JSDocumentationBuilder(PsiElement element, PsiElement _contextElement)
	{
		myElement = element;
		contextElement = _contextElement;
		final PsiElement parent = element.getParent();
		if(element instanceof JSVariable)
		{
			JSExpression initializer = ((JSVariable) element).getInitializer();
			if(initializer instanceof JSFunctionExpression)
			{
				element = initializer;
			}
		}

		if(element instanceof JSFunction)
		{
			function = (JSFunction) element;
			generationInfo = new MethodInfo();

			if(parent instanceof JSClass)
			{
				generationInfo.namespace = ((JSClass) parent).getQualifiedName();
			}

			for(JSParameter parameter : function.getParameterList().getParameters())
			{
				final ParameterInfo paramInfo = new ParameterInfo();
				((MethodInfo) generationInfo).parameterInfoMap.put(parameter.getName(), paramInfo);
				paramInfo.type = parameter.getTypeString();
				paramInfo.initialValue = parameter.getInitializerText();
			}

			((MethodInfo) generationInfo).returnInfo.type = function.getReturnTypeString();
		}
		else if(element instanceof JSNamedElement)
		{
			namedItem = (JSNamedElement) element;
			generationInfo = new SymbolInfo();

			if(namedItem instanceof JSClass)
			{
				if(parent instanceof JSPackageStatement)
				{
					generationInfo.namespace = ((JSPackageStatement) parent).getQualifiedName();
				}
			}
		}
		else
		{
			generationInfo = new SymbolInfo();
		}

		result = generationInfo.description;
	}

	private void doAppend(final @NonNls String str)
	{
		while(myNewLinesPendingCount > 0)
		{
			result.append(seenPre ? "\n" : BR_DELIMITER);
			--myNewLinesPendingCount;
		}

		result.append(str);
	}

	private static final Pattern ourTagStartPattern = Pattern.compile("<(/)?(\\w+)");

	@Override
	public boolean needsPlainCommentData()
	{
		return true;
	}

	@Override
	public boolean onCommentLine(@Nonnull String line)
	{
		String trimmedLine = line.trim();
		boolean parametersStarted = false;
		boolean parametersEnded = false;

		if(generationInfo instanceof MethodInfo)
		{
			final MethodInfo methodInfo = (MethodInfo) generationInfo;
			parametersEnded = methodInfo.parameterCount + 1 == methodInfo.parameterInfoMap.size();
			parametersStarted = methodInfo.parameterCount >= 0;
		}

		if(trimmedLine.length() == 0)
		{
			if(result.length() == 0)
			{
				return true;
			}

			final int maxSubsequentBr = parametersStarted && !parametersEnded ? 0 : 2;
			if(myNewLinesPendingCount < maxSubsequentBr)
			{
				++myNewLinesPendingCount;
			}

			if(parametersEnded && ((MethodInfo) generationInfo).returnInfo.description != result)
			{
				setResult(generationInfo.description);
				myNewLinesPendingCount = 1;
			}

			return true;
		}

		if(line.indexOf("<pre>") != -1)
		{
			seenPre = true;
		}

		if(!seenPre && line.indexOf('<') != -1)
		{
			Matcher matcher = ourTagStartPattern.matcher(line);
			int offset = 0;

			while(matcher.find())
			{
				final boolean isTagEnd = matcher.start(1) != matcher.end(1);
				final String s = matcher.group(2);
				// tags that do not need escaping
				if(tagNameThatDoNotNeedEscaping(s))
				{
					continue;
				}
				line = line.substring(0, offset + matcher.start(0)) + "&lt;" + (isTagEnd ? "/" : "") + s + line.substring(offset + matcher.end(0));
				offset += 3;
			}
		}

		doAppend(line);
		myNewLinesPendingCount = parametersStarted && !parametersEnded ? 0 : 1;

		if(line.indexOf("</pre>") != -1)
		{
			seenPre = false;
		}
		return true;
	}

	private static boolean tagNameThatDoNotNeedEscaping(final @NonNls String s)
	{
		return s.equalsIgnoreCase("p") || s.equalsIgnoreCase("i") || s.equalsIgnoreCase("code") || s.equalsIgnoreCase("ul") ||
				s.equalsIgnoreCase("li") || s.equalsIgnoreCase("b");
	}

	private void setResult(final StringBuilder builder)
	{
		result = builder;
		myNewLinesPendingCount = 0;
	}

	@Override
	public boolean onPatternMatch(@Nonnull MetaDocType metaDocType, @Nullable String matchName, @Nullable String matchValue,
			@Nullable String remainingLineContent, @Nonnull final String line, final String patternMatched)
	{
		if(metaDocType == MetaDocType.DEFAULT)
		{
			boolean color = remainingLineContent.startsWith("0x") && remainingLineContent.length() == 8;
			remainingLineContent = appendCurrentOrDefaultValue(remainingLineContent.substring(color ? 2 : 0), color, true);
		}

		if(metaDocType == MetaDocType.SEE)
		{
			if(!seenSeeAlso)
			{
				seenSeeAlso = true;
				result.append("<DL><DT><b>See also:</b></DT><DD>");
			}
			else
			{
				result.append("</DD><DD>");
			}
			if(URLUtil.isAbsoluteURL(remainingLineContent))
			{
				result.append("<a href=\"").append(remainingLineContent).append("\">").append(remainingLineContent).append("</a>");
			}
			else if(StringUtil.containsAnyChar(remainingLineContent, JSDocumentationProvider.SEE_PLAIN_TEXT_CHARS))
			{
				result.append(StringUtil.stripQuotesAroundValue(remainingLineContent));
			}
			else
			{
				JSDocumentationUtils.appendHyperLinkToElement(null, getSeeAlsoLink(remainingLineContent), result, remainingLineContent, null);
				return true;
			}
		}
		else if(seenSeeAlso)
		{
			seenSeeAlso = false;
			result.append("</DD></DL>");
		}

		if(metaDocType == MetaDocType.EVENT)
		{
			if(myEventsStarted)
			{
				result.append("</DD><DD>");
			}

			result.append("\n");
			if(!myEventsStarted)
			{
				myEventsStarted = true;
				result.append("<DL><DT><b>Events:</b></DT><DD>");
			}
			result.append("Event <code>" + matchName + "</code> -" + remainingLineContent);
			return true;
		}
		else if(myEventsStarted)
		{
			myEventsStarted = false;
			result.append("</DD></DL>");
		}

		if(metaDocType == MetaDocType.NOTE)
		{
			result.append("\n<p><b>Note:</b> " + remainingLineContent);
			return true;
		}

		if(metaDocType == MetaDocType.OPTIONAL_PARAMETERS)
		{
			myOptionalParametersStarted = true;
			if(currentParameterInfo != null)
			{
				currentParameterInfo.optional = true;
			}
			if(remainingLineContent != null)
			{
				onCommentLine(remainingLineContent);
			}
			return true;
		}

		if(metaDocType == MetaDocType.DEPRECATED)
		{
			generationInfo.deprecated = true;
			if(remainingLineContent != null)
			{
				onCommentLine(remainingLineContent);
			}
			return true;
		}

		if(metaDocType == MetaDocType.DESCRIPTION)
		{
			generationInfo.description.append(remainingLineContent);
			return true;
		}

		if(metaDocType == MetaDocType.PRIVATE ||
				metaDocType == MetaDocType.PUBLIC ||
				metaDocType == MetaDocType.PROTECTED ||
				metaDocType == MetaDocType.STATIC)
		{
			final String s = metaDocType.name().toLowerCase();
			if(generationInfo.visibility == null)
			{
				generationInfo.visibility = s;
			}
			else
			{
				generationInfo.visibility += ", " + s;
			}
			return true;
		}
		else if(metaDocType == MetaDocType.TYPE)
		{
			generationInfo.type = matchName;
			return true;
		}
		else if(metaDocType == MetaDocType.FINAL)
		{
			generationInfo.access = "final";
			return true;
		}
		else if(metaDocType == MetaDocType.REQUIRES)
		{
			//onCommentLine("Requires:"+matchName);
			return true;
		}
		else if(metaDocType == MetaDocType.NAMESPACE)
		{
			generationInfo.namespace = matchName;
			return true;
		}

		if(function != null)
		{
			final MethodInfo methodGenerationInfo = ((MethodInfo) generationInfo);

			if(metaDocType == MetaDocType.CONSTRUCTOR)
			{
				methodGenerationInfo.methodType = "contructor";
			}
			else if(metaDocType == MetaDocType.METHOD)
			{
				methodGenerationInfo.methodType = "method";
			}
			else if(metaDocType == MetaDocType.PARAMETER)
			{
				ParameterInfo info = methodGenerationInfo.parameterInfoMap.get(matchName);

				if(info != null)
				{
					int index = 0;
					for(SymbolInfo _info : methodGenerationInfo.parameterInfoMap.values())
					{
						if(info == _info)
						{
							break;
						}
						++index;
					}

					methodGenerationInfo.parameterCount = index;
				}
				else if(patternMatched.indexOf('@') != -1)
				{
					// wrong doc (without parameter name)
					methodGenerationInfo.parameterCount++;
					int index = 0;

					for(SymbolInfo _info : methodGenerationInfo.parameterInfoMap.values())
					{
						if(index == methodGenerationInfo.parameterCount)
						{
							info = (ParameterInfo) _info;
							break;
						}
						++index;
					}
				}

				if(info != null)
				{
					if(matchValue != null)
					{
						info.type = matchValue;
					}
					setResult(info.description);
					info.description.append(remainingLineContent);
					info.optional = myOptionalParametersStarted;
					currentParameterInfo = info;
				}
				else
				{
					onCommentLine(line);
				}
			}
			else if(metaDocType == MetaDocType.RETURN)
			{
				result = methodGenerationInfo.returnInfo.description;
				if(matchName != null)
				{
					methodGenerationInfo.returnInfo.type = matchName;
				}
				if(matchValue != null)
				{
					methodGenerationInfo.returnInfo.description.append(matchValue);
				}
				if(remainingLineContent != null)
				{
					methodGenerationInfo.returnInfo.description.append(remainingLineContent);
				}
			}
		}
		return true;
	}

	private String getSeeAlsoLink(String remainingLineContent)
	{
		if(URLUtil.isAbsoluteURL(remainingLineContent))
		{
			return remainingLineContent;
		}

		if(!remainingLineContent.contains(".") && !remainingLineContent.startsWith("#"))
		{
			// first try to find class in the same package, then in default one
			JSQualifiedNamedElement qualifiedElement = JSDocumentationProvider.findParentQualifiedElement(myElement);
			if(qualifiedElement != null)
			{
				String qname = qualifiedElement.getQualifiedName();
				String aPackage = qname.contains(".") ? qname.substring(0, qname.lastIndexOf('.') + 1) : "";
				String resolvedLink = JSDocumentationProvider.getSeeAlsoLinkResolved(myElement, aPackage + remainingLineContent);
				if(resolvedLink != null)
				{
					return resolvedLink;
				}
			}
		}

		String resolvedLink = JSDocumentationProvider.getSeeAlsoLinkResolved(myElement, remainingLineContent);
		return resolvedLink != null ? resolvedLink : remainingLineContent;
	}

	private String appendCurrentOrDefaultValue(String remainingLineContent, boolean color, boolean defaultValue)
	{
		if(color)
		{
			remainingLineContent = "<span style=\"background-color:#" + remainingLineContent + "\">&nbsp;&nbsp;&nbsp;</span>";
		}
		else
		{
			remainingLineContent = "<code>" + remainingLineContent + "</code>";
		}
		result.append("<br>\n<u>").append(defaultValue ? "Default" : "Current").append(" value:</u>").append(remainingLineContent);
		return remainingLineContent;
	}

	String getDoc()
	{
		if(seenSeeAlso)
		{
			seenSeeAlso = false;
			result.append("</DD></DL>");
		}
		result = new StringBuilder();

		if(function != null)
		{
			startFunction(function);
			result.append(generationInfo.description.toString());

			result.append("\n<DL>");
			final MethodInfo methodInfo = ((MethodInfo) generationInfo);

			if(methodInfo.parameterInfoMap.size() > 0)
			{
				result.append("<DT><b>");
				result.append(CodeInsightBundle.message("javadoc.parameters"));
				result.append("</b></DT>");
			}

			for(Map.Entry<String, ParameterInfo> parameterInfo : methodInfo.parameterInfoMap.entrySet())
			{
				result.append("<DD><code>");
				result.append(parameterInfo.getKey());
				result.append("</code>");

				if(parameterInfo.getValue().description.length() > 0)
				{
					result.append(" - ");
					result.append(parameterInfo.getValue().description.toString());
				}
				result.append("</DD>\n");
			}

			if(methodInfo.returnInfo.description.length() > 0)
			{
				result.append("<DT><b>");
				result.append(CodeInsightBundle.message("javadoc.returns"));
				result.append("</b></DT>");

				result.append("<DD>");
				result.append(methodInfo.returnInfo.description.toString());
				result.append("</DD>");
			}

			result.append("</DL>");
		}
		else
		{
			if(namedItem != null)
			{
				startNamedItem(namedItem.getName());
				endNamedItem();
			}

			result.append(generationInfo.description.toString());
		}

		if(contextElement != null)
		{
			final String text = contextElement.getText();
			if(text.startsWith("#") && text.length() == 7)
			{
				appendCurrentOrDefaultValue(text.substring(1), true, false);
			}
		}

		return result.toString();
	}

	public String getParameterDoc(final String name)
	{
		if(function != null)
		{
			final MethodInfo methodInfo = ((MethodInfo) generationInfo);
			final ParameterInfo parameterInfo = methodInfo.parameterInfoMap.get(name);

			if(parameterInfo != null && parameterInfo.description.length() > 0)
			{
				result = new StringBuilder();
				startNamedItem(name, parameterInfo);
				endNamedItem();
				result.append(parameterInfo.description.toString());
				return result.toString();
			}
		}

		return null;
	}


	private void startFunction(JSFunction function)
	{
		@NonNls String functionName = function.getName();
		final PsiElement parent = function.getParent();

		if(parent instanceof JSAssignmentExpression)
		{
			final String unqualifiedFunctionName = functionName;
			JSExpression expression = ((JSDefinitionExpression) ((JSAssignmentExpression) parent).getLOperand()).getExpression();
			functionName = null;

			if(expression instanceof JSReferenceExpression)
			{
				final JSExpression qualifierExpression = ((JSReferenceExpression) expression).getQualifier();
				if(qualifierExpression instanceof JSReferenceExpression)
				{
					expression = JSSymbolUtil.findReferenceExpressionUsedForClassExtending((JSReferenceExpression) qualifierExpression);
					functionName = expression.getText() + "." + unqualifiedFunctionName;
				}
			}

			if(functionName == null)
			{
				functionName = expression.getText();
			}
			if(generationInfo.namespace != null && functionName.equals(generationInfo.namespace + "." + unqualifiedFunctionName))
			{
				generationInfo.namespace = null;
			}
		}

		if(functionName == null)
		{
			functionName = "<anonymous>";
		}

		int indent = startNamedItem(functionName);

		result.append("(");
		int resultLength = result.length();

		for(Map.Entry<String, ParameterInfo> parameterInfo : ((MethodInfo) generationInfo).parameterInfoMap.entrySet())
		{

			if(result.length() != resultLength)
			{
				result.append(",\n");
				for(int i = 0; i < indent; ++i)
				{
					result.append(" ");
				}
			}
			result.append("&nbsp;");

			String parameterType = parameterInfo.getValue().type;
			if(parameterType != null)
			{
				boolean optional = parameterInfo.getValue().optional;

				if(parameterType.endsWith("?"))
				{
					optional = true;
					parameterType = parameterType.substring(0, parameterType.length() - 1);
				}


				result.append("[ ");
				result.append(parameterType);
				if(optional)
				{
					result.append(", optional");
				}
				result.append(" ] ");
			}

			result.append(parameterInfo.getKey());

			final String initialValue = parameterInfo.getValue().initialValue;
			if(initialValue != null)
			{
				result.append(" = ").append(initialValue);
			}
		}

		result.append("&nbsp;)\n");
		endNamedItem();
	}

	private void endNamedItem()
	{
		result.append("</PRE>");
	}

	private int startNamedItem(final String functionName)
	{
		return startNamedItem(functionName, generationInfo);
	}

	private int startNamedItem(final String functionName, SymbolInfo generationInfo)
	{
		result.append("<PRE>");

		StringBuffer options = new StringBuffer();
		int offset = result.length();

		if(generationInfo instanceof MethodInfo)
		{
			addVisibilityAndAccess(options, generationInfo);
			String returnType = ((MethodInfo) generationInfo).returnInfo.type;
			if(returnType != null)
			{
				if(options.length() > 0)
				{
					options.append(", ");
				}
				options.append(returnType);
			}

			final String type = ((MethodInfo) generationInfo).methodType;
			if(type != null)
			{
				if(options.length() > 0)
				{
					options.append(", ");
				}
				options.append(type);
			}
		}
		else
		{
			if(generationInfo.type != null)
			{
				if(options.length() > 0)
				{
					options.append(", ");
				}
				options.append(generationInfo.type);
			}

			addVisibilityAndAccess(options, generationInfo);
		}

		if(options.length() > 0)
		{
			result.append("[ ");
			result.append(options.toString());
			result.append(" ] ");
		}

		offset = result.length() - offset + functionName.length() + 1;

		result.append("<b>");
		if(generationInfo.namespace != null && generationInfo.namespace.length() > 0)
		{
			result.append(generationInfo.namespace).append('.');
		}
		result.append(functionName);
		result.append("</b>");

		return offset;
	}

	private void addVisibilityAndAccess(final StringBuffer options, SymbolInfo generationInfo)
	{
		if(generationInfo.visibility != null)
		{
			if(options.length() > 0)
			{
				options.append(", ");
			}
			options.append(generationInfo.visibility);
		}

		if(generationInfo.access != null)
		{
			if(options.length() > 0)
			{
				options.append(", ");
			}
			options.append(generationInfo.access);
		}

		if(generationInfo.deprecated)
		{
			if(options.length() > 0)
			{
				options.append(", ");
			}
			options.append("deprecated");
		}
	}
}
