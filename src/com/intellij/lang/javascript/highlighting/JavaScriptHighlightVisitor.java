package com.intellij.lang.javascript.highlighting;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.codeInsight.daemon.impl.HighlightVisitor;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * @author VISTALL
 * @since 26.11.13.
 */
public class JavaScriptHighlightVisitor extends JSElementVisitor implements HighlightVisitor
{
	@NonNls
	private static final String PARAMETER_MESSAGE = "parameter";
	@NonNls
	private static final String INSTANCE_FIELD = "instance field";
	@NonNls
	private static final String INSTANCE_METHOD = "instance method";

	private final boolean myUnitTestMode = ApplicationManager.getApplication().isUnitTestMode();

	private HighlightInfoHolder myHighlightInfoHolder;

	@Override
	public boolean suitableForFile(@NotNull PsiFile psiFile)
	{
		return psiFile instanceof JSFile;
	}

	@Override
	public void visitJSProperty(JSProperty property)
	{
		super.visitJSProperty(property);

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
		myHighlightInfoHolder.add(createLineMarker(property, type, text));
	}

	@Override
	public void visitJSAttribute(JSAttribute jsAttribute)
	{
		super.visitJSAttribute(jsAttribute);

		myHighlightInfoHolder.add(createLineMarker(jsAttribute, JSHighlighter.JS_METADATA, myUnitTestMode ? "attribute" : null));
	}

	@Override
	public void visitJSReferenceExpression(JSReferenceExpression element)
	{
		super.visitJSReferenceExpression(element);

		final ResolveResult[] results = element.multiResolve(false);
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

					isMethod |= (namedItemType == JSNamedElementProxy.NamedItemType.MemberFunction || namedItemType == JSNamedElementProxy.NamedItemType
							.FunctionProperty);
					isFunction |= namedItemType == JSNamedElementProxy.NamedItemType.Function;
					isVariable |= namedItemType == JSNamedElementProxy.NamedItemType.Variable;
					isField |= namedItemType == JSNamedElementProxy.NamedItemType.Definition || namedItemType == JSNamedElementProxy.NamedItemType.Property;

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
					myHighlightInfoHolder.add(buildHighlightForVariable(resolve, element));
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

		myHighlightInfoHolder.add(createLineMarker(element, type, text));
	}

	@Nullable
	private static HighlightInfo buildHighlightForVariable(@NotNull final PsiElement element, @NotNull final PsiElement markerAddTo)
	{
		TextAttributesKey type;
		@NonNls String text;

		if(element instanceof JSParameter)
		{
			type = JSHighlighter.JS_PARAMETER;
			text = PARAMETER_MESSAGE;
		}
		else
		{
			if(isClass(element.getParent().getParent()))
			{
				final JSAttributeList attributeList = ((JSAttributeListOwner) element).getAttributeList();
				final boolean isStatic = attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC);
				type = isStatic ? JSHighlighter.JS_STATIC_MEMBER_VARIABLE : JSHighlighter.JS_INSTANCE_MEMBER_VARIABLE;
				text = (isStatic ? "static " : "") + "field";
			}
			else
			{
				if(PsiTreeUtil.getParentOfType(element, JSFunction.class) != null)
				{
					type = JSHighlighter.JS_LOCAL_VARIABLE;
					text = "local variable";
				}
				else
				{
					type = JSHighlighter.JS_GLOBAL_VARIABLE;
					text = "global variable";
				}
			}
		}

		return createLineMarker(markerAddTo, type, text);
	}

	@Override
	public void visit(@NotNull PsiElement element)
	{
		element.acceptChildren(this);
	}

	@Override
	public boolean analyze(@NotNull PsiFile psiFile, boolean b, @NotNull HighlightInfoHolder highlightInfoHolder, @NotNull Runnable runnable)
	{
		myHighlightInfoHolder = highlightInfoHolder;
		runnable.run();
		return true;
	}

	@NotNull
	@Override
	public HighlightVisitor clone()
	{
		return new JavaScriptHighlightVisitor();
	}

	@Override
	public int order()
	{
		return 0;
	}

	private static boolean isClass(final PsiElement element)
	{
		if(element instanceof JSClass)
		{
			return true;
		}
		if(element instanceof JSFile && element.getContext() != null)
		{
			return true;
		}
		return false;
	}

	@Nullable
	private static HighlightInfo createLineMarker(@NotNull final PsiElement element, @Nullable final TextAttributesKey type,
			@Nullable @NonNls final String text)
	{
		if(type == null)
		{
			return null;
		}
		PsiElement markedNode = element.getLastChild();
		if(element instanceof JSNamedElement)
		{
			ASTNode nameNode = ((JSNamedElement) element).findNameIdentifier();
			if(nameNode != null)
			{
				markedNode = nameNode.getPsi();
			}
		}
		else if(element instanceof JSAttribute)
		{
			markedNode = element;
		}

		if(markedNode == null)
		{
			return null;
		}

		HighlightInfo.Builder builder = HighlightInfo.newHighlightInfo(HighlightInfoType.INFORMATION).range(markedNode).textAttributes(type);

		if(text != null)
		{
			builder = builder.descriptionAndTooltip(text);
		}

		return builder.create();
	}
}
