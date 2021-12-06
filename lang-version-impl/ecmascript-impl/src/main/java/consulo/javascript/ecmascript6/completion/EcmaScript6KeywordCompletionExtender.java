package consulo.javascript.ecmascript6.completion;

import com.intellij.codeInsight.completion.AddSpaceInsertHandler;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.util.ProcessingContext;
import consulo.javascript.ide.completion.JavaScriptKeywordCompletionExtender;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class EcmaScript6KeywordCompletionExtender implements JavaScriptKeywordCompletionExtender
{
	@Override
	public void fillCompletion(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result)
	{
		result.addElement(LookupElementBuilder.create("let").withInsertHandler(AddSpaceInsertHandler.INSTANCE).bold());
	}
}
