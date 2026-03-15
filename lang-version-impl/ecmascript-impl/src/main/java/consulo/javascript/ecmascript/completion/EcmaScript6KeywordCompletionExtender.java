package consulo.javascript.ecmascript.completion;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.completion.CompletionResultSet;
import consulo.language.editor.completion.lookup.AddSpaceInsertHandler;
import consulo.language.editor.completion.CompletionParameters;
import consulo.language.editor.completion.lookup.LookupElementBuilder;
import consulo.language.util.ProcessingContext;
import consulo.javascript.ide.completion.JavaScriptKeywordCompletionExtender;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
@ExtensionImpl
public class EcmaScript6KeywordCompletionExtender implements JavaScriptKeywordCompletionExtender {
    @Override
    public void fillCompletion(CompletionParameters parameters, ProcessingContext context, CompletionResultSet result) {
        result.addElement(LookupElementBuilder.create("let").withInsertHandler(AddSpaceInsertHandler.INSTANCE).bold());
    }
}
