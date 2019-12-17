package consulo.javascript.jsx.fileTemplate;

import com.intellij.ide.fileTemplates.DefaultCreateFromTemplateHandler;
import com.intellij.ide.fileTemplates.FileTemplate;
import com.intellij.lang.javascript.JavaScriptFileType;
import consulo.javascript.jsx.language.JSXFileType;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class JSXCreateFromTemplateHandler extends DefaultCreateFromTemplateHandler
{
	@Override
	public boolean handlesTemplate(FileTemplate template)
	{
		return template.getExtension().equals(JavaScriptFileType.INSTANCE.getDefaultExtension());
	}

	@Override
	protected String checkAppendExtension(String fileName, FileTemplate template)
	{
		if(fileName.endsWith("." + JSXFileType.INSTANCE.getDefaultExtension()))
		{
			return fileName;
		}
		return super.checkAppendExtension(fileName, template);
	}
}
