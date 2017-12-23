package consulo.javascript.formatter;

import org.jetbrains.annotations.NotNull;
import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.psi.codeStyle.CodeStyleSettings;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
public class JavaScriptCodeStyleConfigurable extends CodeStyleAbstractConfigurable
{
	public JavaScriptCodeStyleConfigurable(@NotNull CodeStyleSettings settings, CodeStyleSettings cloneSettings)
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