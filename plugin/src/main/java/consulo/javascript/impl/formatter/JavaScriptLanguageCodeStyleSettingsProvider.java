package consulo.javascript.impl.formatter;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.codeStyle.CommonCodeStyleSettings;
import consulo.language.codeStyle.setting.CodeStyleSettingsCustomizable;
import consulo.language.codeStyle.setting.IndentOptionsEditor;
import consulo.language.codeStyle.setting.LanguageCodeStyleSettingsProvider;
import consulo.language.codeStyle.ui.setting.SmartIndentOptionsEditor;
import consulo.util.io.FileUtil;

import jakarta.annotation.Nonnull;
import java.io.IOException;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
@ExtensionImpl
public class JavaScriptLanguageCodeStyleSettingsProvider extends LanguageCodeStyleSettingsProvider
{
	@Override
	public IndentOptionsEditor getIndentOptionsEditor()
	{
		return new SmartIndentOptionsEditor();
	}

	@Override
	public CommonCodeStyleSettings getDefaultCommonSettings()
	{
		CommonCodeStyleSettings defaultSettings = new CommonCodeStyleSettings(getLanguage());
		CommonCodeStyleSettings.IndentOptions indentOptions = defaultSettings.initIndentOptions();
		indentOptions.INDENT_SIZE = 2;
		indentOptions.CONTINUATION_INDENT_SIZE = 4;
		indentOptions.TAB_SIZE = 2;

		defaultSettings.KEEP_LINE_BREAKS = false;

		return defaultSettings;
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Override
	public void customizeSettings(@Nonnull CodeStyleSettingsCustomizable consumer, @Nonnull SettingsType settingsType)
	{
		if(settingsType == SettingsType.SPACING_SETTINGS)
		{
			consumer.showStandardOptions("SPACE_BEFORE_METHOD_CALL_PARENTHESES", "SPACE_BEFORE_METHOD_PARENTHESES", "SPACE_BEFORE_IF_PARENTHESES",
					"SPACE_BEFORE_WHILE_PARENTHESES", "SPACE_BEFORE_FOR_PARENTHESES", "SPACE_BEFORE_CATCH_PARENTHESES",
					"SPACE_BEFORE_SWITCH_PARENTHESES", "SPACE_AROUND_ASSIGNMENT_OPERATORS", "SPACE_AROUND_LOGICAL_OPERATORS",
					"SPACE_AROUND_EQUALITY_OPERATORS", "SPACE_AROUND_RELATIONAL_OPERATORS", "SPACE_AROUND_ADDITIVE_OPERATORS",
					"SPACE_AROUND_MULTIPLICATIVE_OPERATORS", "SPACE_AROUND_SHIFT_OPERATORS", "SPACE_BEFORE_CLASS_LBRACE",
					"SPACE_BEFORE_METHOD_LBRACE", "SPACE_BEFORE_IF_LBRACE", "SPACE_BEFORE_ELSE_LBRACE", "SPACE_BEFORE_WHILE_LBRACE",
					"SPACE_BEFORE_FOR_LBRACE", "SPACE_BEFORE_SWITCH_LBRACE", "SPACE_BEFORE_TRY_LBRACE", "SPACE_BEFORE_CATCH_LBRACE",
					"SPACE_BEFORE_WHILE_KEYWORD", "SPACE_BEFORE_ELSE_KEYWORD", "SPACE_BEFORE_CATCH_KEYWORD", "SPACE_WITHIN_METHOD_CALL_PARENTHESES",
					"SPACE_WITHIN_METHOD_PARENTHESES", "SPACE_WITHIN_IF_PARENTHESES", "SPACE_WITHIN_WHILE_PARENTHESES",
					"SPACE_WITHIN_FOR_PARENTHESES", "SPACE_WITHIN_CATCH_PARENTHESES", "SPACE_WITHIN_SWITCH_PARENTHESES", "SPACE_BEFORE_QUEST",
					"SPACE_AFTER_QUEST", "SPACE_BEFORE_COLON", "SPACE_AFTER_COLON", "SPACE_AFTER_COMMA", "SPACE_AFTER_COMMA_IN_TYPE_ARGUMENTS",
					"SPACE_BEFORE_COMMA", "SPACE_AROUND_UNARY_OPERATOR", "SPACE_WITHIN_BRACKETS", "SPACE_BEFORE_METHOD_PARENTHESES");
		}
		else if(settingsType == SettingsType.BLANK_LINES_SETTINGS)
		{
			consumer.showStandardOptions("KEEP_BLANK_LINES_IN_CODE");
		}
		else if(settingsType == SettingsType.WRAPPING_AND_BRACES_SETTINGS)
		{
			consumer.showStandardOptions("KEEP_LINE_BREAKS", "KEEP_FIRST_COLUMN_COMMENT", "BRACE_STYLE", "CLASS_BRACE_STYLE", "METHOD_BRACE_STYLE",
					"CALL_PARAMETERS_WRAP", "CALL_PARAMETERS_LPAREN_ON_NEXT_LINE", "CALL_PARAMETERS_RPAREN_ON_NEXT_LINE", "METHOD_PARAMETERS_WRAP",
					"METHOD_PARAMETERS_LPAREN_ON_NEXT_LINE", "METHOD_PARAMETERS_RPAREN_ON_NEXT_LINE", "ELSE_ON_NEW_LINE", "WHILE_ON_NEW_LINE",
					"CATCH_ON_NEW_LINE", "ALIGN_MULTILINE_PARAMETERS", "ALIGN_MULTILINE_PARAMETERS_IN_CALLS", "ALIGN_MULTILINE_BINARY_OPERATION",
					"BINARY_OPERATION_WRAP", "BINARY_OPERATION_SIGN_ON_NEXT_LINE", "TERNARY_OPERATION_WRAP", "TERNARY_OPERATION_SIGNS_ON_NEXT_LINE",
					"PARENTHESES_EXPRESSION_LPAREN_WRAP", "PARENTHESES_EXPRESSION_RPAREN_WRAP", "ALIGN_MULTILINE_TERNARY_OPERATION");
		}
	}

	@Override
	public String getCodeSample(@Nonnull SettingsType settingsType)
	{
		return loadPreview("codeStyle.txt");
	}

	private static String loadPreview(String file)
	{
		try
		{
			return FileUtil.loadTextAndClose(JavaScriptLanguageCodeStyleSettingsProvider.class.getResourceAsStream("/codeStyle/" + file));
		}
		catch(IOException e)
		{
			throw new Error(e);
		}
	}
}
