package com.intellij.lang.javascript;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.fileTypes.LanguageFileType;

/**
 * @author VISTALL
 * @since 23.12.13.
 */
public class EcmaScriptFileType extends LanguageFileType
{
	public static final EcmaScriptFileType INSTANCE = new EcmaScriptFileType();

	private EcmaScriptFileType()
	{
		super(JavaScriptSupportLoader.ECMA_SCRIPT_L4);
	}

	@NotNull
	@Override
	public String getName()
	{
		return "ECMASCRIPT";
	}

	@NotNull
	@Override
	public String getDescription()
	{
		return "EcmaScript files";
	}

	@NotNull
	@Override
	public String getDefaultExtension()
	{
		return "es";
	}

	@Nullable
	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.JavaScript;
	}
}
