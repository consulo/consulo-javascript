package com.intellij.lang.javascript.flex;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.fileTypes.LanguageFileType;

/**
 * @author VISTALL
 * @since 02.12.13.
 */
public class ActionScriptFileType extends LanguageFileType
{
	public static final ActionScriptFileType INSTANCE = new ActionScriptFileType();

	private ActionScriptFileType()
	{
		super(JavaScriptSupportLoader.ECMA_SCRIPT_L4);
	}

	@NotNull
	@Override
	public String getName()
	{
		return "ACTIONSCRIPT";
	}

	@NotNull
	@Override
	public String getDescription()
	{
		return "ActionScript files";
	}

	@NotNull
	@Override
	public String getDefaultExtension()
	{
		return "as";
	}

	@Nullable
	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.As;
	}
}
