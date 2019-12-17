package consulo.javascript.jsx.language.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.IncorrectOperationException;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class JSXXmlTagManipulator extends AbstractElementManipulator<XmlTag>
{
	@Override
	public XmlTag handleContentChange(@Nonnull XmlTag tag, @Nonnull TextRange textRange, String s) throws IncorrectOperationException
	{
		return null;
	}
}
