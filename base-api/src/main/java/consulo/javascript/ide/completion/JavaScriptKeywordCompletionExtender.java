package consulo.javascript.ide.completion;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.component.extension.ExtensionPointName;
import consulo.language.editor.completion.CompletionParameters;
import consulo.language.editor.completion.CompletionResultSet;
import consulo.language.util.ProcessingContext;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public interface JavaScriptKeywordCompletionExtender
{
	ExtensionPointName<JavaScriptKeywordCompletionExtender> EP_NAME = ExtensionPointName.create(JavaScriptKeywordCompletionExtender.class);

	void fillCompletion(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result);
}
