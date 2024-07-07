package consulo.javascript.impl.formatter;

import jakarta.annotation.Nonnull;

import consulo.language.codeStyle.ui.setting.CodeStyleAbstractConfigurable;
import consulo.language.codeStyle.CodeStyleSettings;
import consulo.language.codeStyle.ui.setting.CodeStyleAbstractPanel;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
public class JavaScriptCodeStyleConfigurable extends CodeStyleAbstractConfigurable
{
	public JavaScriptCodeStyleConfigurable(@Nonnull CodeStyleSettings settings, CodeStyleSettings cloneSettings)
	{
		super(settings, cloneSettings, "JavaScript");
	}

	@Override
	protected CodeStyleAbstractPanel createPanel(CodeStyleSettings settings)
	{
		return new JavaScriptCodeStylePanel(getCurrentSettings(), settings);
	}

	@Override
	public String getHelpTopic()
	{
		return "reference.settingsdialog.codestyle.javascript";
	}
}