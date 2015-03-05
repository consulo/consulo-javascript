/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.formatter;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.EditorHighlighterFactory;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.ui.DocumentAdapter;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 12, 2008
 *         Time: 10:39:21 PM
 */
public class JSCodeStylePanel extends CodeStyleAbstractPanel
{
	private JPanel myPanel;
	private JCheckBox myIndentPackageChildren;
	private JPanel myPreviewPanel;
	private JTextField myFieldPrefixTextField;
	private JTextField myPropertyPrefixTextField;
	private JCheckBox myUseSemicolon;
	private boolean myInsideUpdate = false;

	public JSCodeStylePanel(final CodeStyleSettings settings)
	{
		super(settings);

		installPreviewPanel(myPreviewPanel);
		addPanelToWatch(myPanel);

		myUseSemicolon.addItemListener(new ItemListener()
		{
			@Override
			public void itemStateChanged(final ItemEvent e)
			{
				if(!myInsideUpdate)
				{
					//updatePreviewEditor();
					somethingChanged();
				}
			}
		});

		final DocumentAdapter adapter = new DocumentAdapter()
		{
			@Override
			protected void textChanged(final DocumentEvent e)
			{
				if(!myInsideUpdate)
				{
					//updatePreviewEditor();
					somethingChanged();
				}
			}
		};

		myFieldPrefixTextField.getDocument().addDocumentListener(adapter);
		myPropertyPrefixTextField.getDocument().addDocumentListener(adapter);
	}

	@Override
	protected EditorHighlighter createHighlighter(final EditorColorsScheme scheme)
	{
		return EditorHighlighterFactory.getInstance().createEditorHighlighter(new LightVirtualFile("a.as"), scheme, null);
	}

	@Override
	protected int getRightMargin()
	{
		return 60;
	}

	@Override
	protected void prepareForReformat(final PsiFile psiFile)
	{
	}

	@Override
	@NotNull
	protected FileType getFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@Override
	protected String getPreviewText()
	{
		final JSCodeStyleSettings jsCodeStyleSettings = getSettings().getCustomSettings(JSCodeStyleSettings.class);
		@NonNls String baseName = "field";
		@NonNls String propertyName = (myPropertyPrefixTextField != null ? myPropertyPrefixTextField.getText() : jsCodeStyleSettings.PROPERTY_PREFIX) +
				baseName;
		@NonNls String varName = (myFieldPrefixTextField != null ? myFieldPrefixTextField.getText() : jsCodeStyleSettings.FIELD_PREFIX) + baseName;
		@NonNls String semiColon = (myUseSemicolon != null ? myUseSemicolon.isSelected() : jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT) ? ";" : "";

		return "package aaa {\nclass XXX {\n" +
				"private var " + varName + semiColon + "\n" +
				"function get " + propertyName + "() {\nreturn " + varName + semiColon + "\n" +
				"}\n}\n}";
	}

	@Override
	public void apply(final CodeStyleSettings settings)
	{
		final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
		jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN = myIndentPackageChildren.isSelected() ? JSCodeStyleSettings.INDENT : JSCodeStyleSettings
				.DO_NOT_INDENT;
		jsCodeStyleSettings.FIELD_PREFIX = myFieldPrefixTextField.getText();
		jsCodeStyleSettings.PROPERTY_PREFIX = myPropertyPrefixTextField.getText();
		jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT = myUseSemicolon.isSelected();
	}

	@Override
	public boolean isModified(final CodeStyleSettings settings)
	{
		final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
		return (jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT) != myIndentPackageChildren.isSelected() ||
				!jsCodeStyleSettings.FIELD_PREFIX.equals(myFieldPrefixTextField.getText()) ||
				!jsCodeStyleSettings.PROPERTY_PREFIX.equals(myPropertyPrefixTextField.getText()) ||
				jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT != (myUseSemicolon.isSelected());
	}

	@Override
	public JComponent getPanel()
	{
		return myPanel;
	}

	@Override
	protected void resetImpl(final CodeStyleSettings settings)
	{
		try
		{
			myInsideUpdate = true;
			final JSCodeStyleSettings jsCodeStyleSettings = settings.getCustomSettings(JSCodeStyleSettings.class);
			myIndentPackageChildren.setSelected(jsCodeStyleSettings.INDENT_PACKAGE_CHILDREN == JSCodeStyleSettings.INDENT);
			myFieldPrefixTextField.setText(jsCodeStyleSettings.FIELD_PREFIX);
			myPropertyPrefixTextField.setText(jsCodeStyleSettings.PROPERTY_PREFIX);
			myUseSemicolon.setSelected(jsCodeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT);
		}
		finally
		{
			myInsideUpdate = false;
		}
	}

	@Override
	protected String getFileTypeExtension(final FileType fileType)
	{
		return "js2";
	}
}
