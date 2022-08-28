package consulo.javascript.ide.completion;

import consulo.component.extension.ExtensionPointName;
import consulo.language.editor.completion.CompletionParameters;
import consulo.language.editor.completion.CompletionResultSet;
import consulo.language.util.ProcessingContext;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public interface JavaScriptKeywordCompletionExtender
{
	ExtensionPointName<JavaScriptKeywordCompletionExtender> EP_NAME = ExtensionPointName.create("consulo.javascript.keywordCompletionExtender");

	void fillCompletion(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result);
}
