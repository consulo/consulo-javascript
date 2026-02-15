package consulo.javascript.debugger.browser;

import consulo.configurable.ConfigurationException;
import consulo.execution.configuration.ui.SettingsEditor;
import consulo.localize.LocalizeValue;
import consulo.ui.Component;
import consulo.ui.TextBox;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.util.FormBuilder;
import jakarta.annotation.Nullable;

import java.util.Objects;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfigurationEditor extends SettingsEditor<JavaScriptBrowserConfiguration> {
    private TextBox myUrlBox;

    private FormBuilder myFormBuilder = FormBuilder.create();

    private Component myRoot;

    public JavaScriptBrowserConfigurationEditor() {
        myUrlBox = TextBox.create();

        myFormBuilder.addLabeled(LocalizeValue.localizeTODO("URL:"), myUrlBox);

        myRoot = myFormBuilder.build();
    }

    @Nullable
    @Override
    @RequiredUIAccess
    protected Component createUIComponent() {
        return myRoot;
    }

    @Override
    protected void resetEditorFrom(JavaScriptBrowserConfiguration javaScriptBrowserConfiguration) {
        myUrlBox.setValue(Objects.requireNonNullElse(javaScriptBrowserConfiguration.URL, ""));
    }

    @Override
    protected void applyEditorTo(JavaScriptBrowserConfiguration javaScriptBrowserConfiguration) throws ConfigurationException {
        javaScriptBrowserConfiguration.URL = myUrlBox.getValue();
    }
}
