package com.intellij.lang.javascript;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.fileTypes.LanguageFileType;

/**
 * @author VISTALL
 * @since 02.12.13.
 */
public class JsonFileType extends LanguageFileType
{
	public static final JsonFileType INSTANCE = new JsonFileType();

	private JsonFileType()
	{
		super(JavaScriptSupportLoader.JSON);
	}

	@NotNull
	@Override
	public String getName()
	{
		return "JSON";
	}

	@NotNull
	@Override
	public String getDescription()
	{
		return "Jsone files";
	}

	@NotNull
	@Override
	public String getDefaultExtension()
	{
		return "json";
	}

	@Nullable
	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.Json;
	}
}
