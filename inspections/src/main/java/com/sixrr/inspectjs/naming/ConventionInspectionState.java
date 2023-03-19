package com.sixrr.inspectjs.naming;

import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.configurable.ConfigurableBuilder;
import consulo.configurable.ConfigurableBuilderState;
import consulo.configurable.UnnamedConfigurable;
import consulo.ui.Label;
import consulo.util.xml.serializer.XmlSerializerUtil;
import consulo.util.xml.serializer.annotation.Transient;

import javax.annotation.Nullable;
import java.util.regex.Pattern;

/**
 * @author VISTALL
 * @since 18/03/2023
 */
public class ConventionInspectionState<T extends ConventionInspectionState<T>>
{
	public String m_regex;

	public int m_minLength;

	public int m_maxLength;

	@Transient
	private Pattern m_regexPattern;

	public ConventionInspectionState()
	{
	}

	public ConventionInspectionState(String regex, int minLength, int maxLength)
	{
		this.m_regex = regex;
		this.m_minLength = minLength;
		this.m_maxLength = maxLength;
	}

	public String getRegex()
	{
		return m_regex;
	}

	public void setRegex(String m_regex)
	{
		this.m_regex = m_regex;
	}

	public int getMinLength()
	{
		return m_minLength;
	}

	public void setMinLength(int m_minLength)
	{
		this.m_minLength = m_minLength;
	}

	public int getMaxLength()
	{
		return m_maxLength;
	}

	public void setMaxLength(int m_maxLength)
	{
		this.m_maxLength = m_maxLength;
	}

	public Pattern getPattern()
	{
		if(m_regexPattern == null)
		{
			m_regexPattern = Pattern.compile(m_regex);
		}
		return m_regexPattern;
	}

	@SuppressWarnings("unchecked")
	public T getState()
	{
		return (T) this;
	}

	public void loadState(T state)
	{
		XmlSerializerUtil.copyBean(state, this);

		m_regexPattern = null;
	}

	@Nullable
	public UnnamedConfigurable createConfigurable()
	{
		ConfigurableBuilder<ConfigurableBuilderState> builder = ConfigurableBuilder.newBuilder();
		builder.component(() -> Label.create(InspectionJSLocalize.patternParameter()));
		builder.textBox(() -> m_regex, s -> m_regex = s);
		builder.component(() -> Label.create(InspectionJSLocalize.minLengthParameter()));
		builder.intBox(() -> m_minLength, value -> m_minLength = value);
		builder.component(() -> Label.create(InspectionJSLocalize.maxLengthParameter()));
		builder.intBox(() -> m_maxLength, value -> m_maxLength = value);
		return builder.buildUnnamed();
	}
}
