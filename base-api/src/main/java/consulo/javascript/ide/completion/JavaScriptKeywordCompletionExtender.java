package consulo.javascript.ide.completion;

import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.util.ProcessingContext;

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
