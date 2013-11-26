package com.intellij.lang.javascript.highlighting;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.impl.HighlightVisitor;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightInfoHolder;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSAttributeListOwner;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
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

	private HighlightInfoHolder myHighlightInfoHolder;

	@Override
	public boolean suitableForFile(@NotNull PsiFile psiFile)
	{
		return psiFile instanceof JSFile;
	}

	@Override
	public void visitJSVariable(JSVariable node)
	{
		super.visitJSVariable(node);
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

	@Nullable
	private static com.intellij.codeInsight.daemon.LineMarkerInfo buildHighlightForVariable(@NotNull final PsiElement element,
			@NotNull final PsiElement markerAddTo)
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
	private static LineMarkerInfo createLineMarker(@NotNull final PsiElement element, @Nullable final TextAttributesKey type,
			@Nullable @NonNls final String text)
	{
   /* if (type == null) return null;
	PsiElement markedNode = element.getLastChild();
    if (element instanceof JSNamedElement) {
      ASTNode nameNode = ((JSNamedElement)element).findNameIdentifier();
      if (nameNode != null) markedNode = nameNode.getPsi();
    } else if (element instanceof JSAttribute) {
      markedNode = element;
    }

    if (markedNode == null) return null;
    final LineMarkerInfo markerInfo = new LineMarkerInfo<PsiElement>(
        markedNode,
        markedNode.getTextOffset(),
        null,
        Pass.UPDATE_ALL,
        text != null ? new Function<PsiElement, String>() {
          public String fun(final PsiElement psiElement) {
            return text;
          }
        } : null,
        null,
        null
    );
    markerInfo.endOffset = markerInfo.startOffset + markedNode.getTextLength();
    markerInfo.textAttributesKey = type;
    return markerInfo;  */
		return null;
	}
}
