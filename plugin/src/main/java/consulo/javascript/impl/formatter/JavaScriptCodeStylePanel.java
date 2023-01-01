package consulo.javascript.impl.formatter;

import consulo.language.codeStyle.CodeStyleSettings;
import consulo.language.codeStyle.ui.setting.TabbedLanguageCodeStylePanel;
import com.intellij.lang.javascript.impl.formatter.JSCodeStylePanel;
import consulo.javascript.language.JavaScriptLanguage;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
public class JavaScriptCodeStylePanel extends TabbedLanguageCodeStylePanel
{
	public JavaScriptCodeStylePanel(CodeStyleSettings currentSettings, CodeStyleSettings cloneSettings)
	{
		super(JavaScriptLanguage.INSTANCE, currentSettings, cloneSettings);
	}

	@Override
	protected void initTabs(CodeStyleSettings settings)
	{
		super.initTabs(settings);
		addTab(new JSCodeStylePanel(settings));
	}
}
