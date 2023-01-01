package consulo.javascript.jsx.fileTemplate;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFileType;
import consulo.fileTemplate.CreateFromTemplateHandler;
import consulo.fileTemplate.FileTemplate;
import consulo.javascript.jsx.language.JSXFileType;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
@ExtensionImpl
public class JSXCreateFromTemplateHandler implements CreateFromTemplateHandler
{
	@Override
	public boolean handlesTemplate(FileTemplate template)
	{
		return template.getExtension().equals(JavaScriptFileType.INSTANCE.getDefaultExtension());
	}

	@Nonnull
	@Override
	public String checkAppendExtension(String fileName, FileTemplate template)
	{
		if(fileName.endsWith("." + JSXFileType.INSTANCE.getDefaultExtension()))
		{
			return fileName;
		}
		return CreateFromTemplateHandler.super.checkAppendExtension(fileName, template);
	}
}
