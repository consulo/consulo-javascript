package consulo.javascript.debugger.browser;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import consulo.ui.Component;
import consulo.ui.Label;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.layout.VerticalLayout;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfigurationEditor extends SettingsEditor<JavaScriptBrowserConfiguration>
{
	@Nullable
	@Override
	@RequiredUIAccess
	protected Component createUIComponent()
	{
		return VerticalLayout.create().add(Label.create("test"));
	}

	@Override
	protected void resetEditorFrom(JavaScriptBrowserConfiguration javaScriptBrowserConfiguration)
	{

	}

	@Override
	protected void applyEditorTo(JavaScriptBrowserConfiguration javaScriptBrowserConfiguration) throws ConfigurationException
	{

	}
}
