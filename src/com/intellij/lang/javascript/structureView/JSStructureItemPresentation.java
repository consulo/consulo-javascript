package com.intellij.lang.javascript.structureView;

import gnu.trove.THashMap;

import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.intellij.icons.AllIcons;
import com.intellij.ide.IconDescriptorUpdaters;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlTag;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 23, 2008
 *         Time: 6:54:27 PM
 */
class JSStructureItemPresentation extends JSStructureViewElement.JSStructureItemPresentationBase
{
	public JSStructureItemPresentation(final JSStructureViewElement jsStructureViewElement)
	{
		super(jsStructureViewElement);
	}

	public String getPresentableText()
	{
		PsiElement psiElement = element.getUpToDateElement();
		if(psiElement == null || !psiElement.isValid())
		{
			return "*invalid*";
		}

		if(psiElement instanceof JSObjectLiteralExpression)
		{
			if(psiElement.getParent() instanceof JSAssignmentExpression)
			{
				final JSExpression expression = ((JSDefinitionExpression) ((JSAssignmentExpression) psiElement.getParent()).getLOperand()).getExpression();
				return JSResolveUtil.findClassIdentifier(expression).getText();
			}
			else
			{
				return JSBundle.message("javascript.language.term.prototype");
			}
		}

		if(psiElement instanceof JSDefinitionExpression)
		{
			psiElement = ((JSDefinitionExpression) psiElement).getExpression();
		}

		if(psiElement instanceof JSReferenceExpression)
		{
			JSReferenceExpression expression = (JSReferenceExpression) psiElement;

			if(element.getProxy() != null && element.getProxy().getType() == JSNamedElementProxy.NamedItemType.Namespace)
			{
				final JSExpression jsExpression = expression.getQualifier();
				if(jsExpression instanceof JSReferenceExpression)
				{

					expression = (JSReferenceExpression) jsExpression;
				}
			}

			String s = expression.getReferencedName();

			if(JSResolveUtil.PROTOTYPE_FIELD_NAME.equals(s))
			{
				final JSExpression jsExpression = expression.getQualifier();
				if(jsExpression instanceof JSReferenceExpression)
				{
					s = ((JSReferenceExpression) jsExpression).getReferencedName();
				}
			}
			return s;
		}

		if(!(psiElement instanceof PsiNamedElement))
		{
			return psiElement.getText();
		}

		String name = ((PsiNamedElement) psiElement).getName();

		if(psiElement instanceof JSProperty)
		{
			psiElement = ((JSProperty) psiElement).getValue();
		}

		if(psiElement instanceof JSFunction)
		{
			if(name == null)
			{
				name = "<anonymous>";
			}
			name += "(";
			JSParameterList parameterList = ((JSFunction) psiElement).getParameterList();
			if(parameterList != null)
			{
				for(JSParameter p : parameterList.getParameters())
				{
					if(!name.endsWith("("))
					{
						name += ", ";
					}
					name += p.getName();
					final String variableType = p.getTypeString();
					if(variableType != null)
					{
						name += ":" + variableType;
					}
				}
			}
			name += ")";

			final String type = ((JSFunction) psiElement).getReturnTypeString();
			if(type != null)
			{
				name += ":" + type;
			}
		}

		if(name == null && psiElement.getParent() instanceof JSAssignmentExpression)
		{
			JSExpression lOperand = ((JSDefinitionExpression) ((JSAssignmentExpression) psiElement.getParent()).getLOperand()).getExpression();
			lOperand = JSResolveUtil.findClassIdentifier(lOperand);
			if(lOperand instanceof JSReferenceExpression)
			{
				return ((JSReferenceExpression) lOperand).getReferencedName();
			}
			return lOperand.getText();
		}
		return name;
	}

	public Icon getIcon(boolean open)
	{
		final PsiElement psiElement = this.element.getRealElement();
		if(!psiElement.isValid())
		{
			return null;
		}
		if(psiElement instanceof JSProperty)
		{
			final JSExpression expression = ((JSProperty) psiElement).getValue();
			if(expression instanceof JSObjectLiteralExpression)
			{
				return AllIcons.Nodes.Class;
			}
			if(expression instanceof JSFunction)
			{
				return AllIcons.Nodes.Method;
			}
			return AllIcons.Nodes.Variable;
		}
		if(psiElement instanceof JSReferenceExpression)
		{
			return AllIcons.Nodes.Variable;
		}

		if(psiElement instanceof JSNamedElementProxy && ((JSNamedElementProxy) psiElement).getType() == JSNamedElementProxy.NamedItemType.AttributeValue)
		{
			final Icon icon = getIcon(PsiTreeUtil.getParentOfType(((JSNamedElementProxy) psiElement).getElement(), XmlTag.class));
			if(icon != null)
			{
				return icon;
			}
		}

		if(element.getProxy() != null)
		{
			return IconDescriptorUpdaters.getIcon(element.getProxy(), 0);
		}
		return IconDescriptorUpdaters.getIcon(psiElement, 0);
	}

	private static Map<String, Icon> myQNameToIconMap = new THashMap<String, Icon>();
	private static long myQNameToIconModificationCount;

	private Icon getIcon(XmlTag tag)
	{
		if(tag == null)
		{
			return null;
		}

		if(JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile()))
		{
			Icon icon;
			final long count = tag.getManager().getModificationTracker().getModificationCount();
			final String tagName = tag.getName();

			if(myQNameToIconModificationCount == count)
			{
				icon = myQNameToIconMap.get(tagName);
				if(icon != null)
				{
					return icon;
				}
				if(myQNameToIconMap.containsKey(tagName))
				{
					return null;
				}
			}
			else
			{
				myQNameToIconMap.clear();
				myQNameToIconModificationCount = count;
			}

			myQNameToIconMap.put(tagName, icon = findIcon(tag));
			return icon;
		}

		return null;
	}

	private static Icon findIcon(XmlTag tag)
	{
		final JSClass aClass = JSResolveUtil.getClassFromTagNameInMxml(tag.getFirstChild());
		if(aClass != null)
		{
			final JSAttributeList attributeList = aClass.getAttributeList();

			if(attributeList != null)
			{
				final JSAttribute[] attrs = attributeList.getAttributesByName("IconFile");

				if(attrs.length > 0)
				{
					final JSAttributeNameValuePair pair = attrs[0].getValueByName(null);

					if(pair != null)
					{
						final String s = pair.getSimpleValue();
						final VirtualFile file = aClass.getContainingFile().getVirtualFile();

						if(file != null)
						{
							final VirtualFile parent = file.getParent();
							final VirtualFile child = parent != null ? parent.findChild(s) : null;
							if(child != null)
							{
								return new ImageIcon(child.getPath());
							}
						}
					}
				}
			}
		}

		return null;
	}
}
