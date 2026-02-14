/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.impl.ide.completion;

import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ide.completion.JavaScriptKeywordCompletionExtender;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.completion.*;
import consulo.language.editor.completion.lookup.LookupElementBuilder;
import consulo.language.editor.completion.lookup.ParenthesesInsertHandler;
import consulo.language.pattern.StandardPatterns;
import consulo.language.psi.PsiElement;
import consulo.language.util.ProcessingContext;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 20.12.2015
 */
@ExtensionImpl
public class JavaScriptKeywordCompletionContributor extends CompletionContributor {
    public JavaScriptKeywordCompletionContributor() {
        extend(
            CompletionType.BASIC,
            StandardPatterns.psiElement().withParent(JSReferenceExpression.class),
            new CompletionProvider() {
                @RequiredReadAction
                @Override
                public void addCompletions(
                    @Nonnull CompletionParameters parameters,
                    ProcessingContext context,
                    @Nonnull CompletionResultSet result
                ) {
                    PsiElement position = parameters.getPosition();
                    JSReferenceExpression parent = (JSReferenceExpression)position.getParent();
                    if (parent.getQualifier() != null) {
                        return;
                    }

                    boolean parentIsStatement = parent.getParent() instanceof JSExpressionStatement;

                    LookupElementBuilder functionKeyword = LookupElementBuilder.create("function");
                    functionKeyword = functionKeyword.bold();
                    if (parentIsStatement) {
                        functionKeyword = functionKeyword.withInsertHandler(SpaceInsertHandler.INSTANCE);
                    }
                    else {
                        functionKeyword = functionKeyword.withInsertHandler(ParenthesesInsertHandler.getInstance(false));
                        functionKeyword = functionKeyword.withPresentableText("function()");
                    }

                    result.addElement(functionKeyword);

                    result.addElement(LookupElementBuilder.create("var").withInsertHandler(SpaceInsertHandler.INSTANCE).bold());
                    result.addElement(LookupElementBuilder.create("const").withInsertHandler(SpaceInsertHandler.INSTANCE).bold());

                    result.addElement(LookupElementBuilder.create("if").bold());
                    result.addElement(LookupElementBuilder.create("for").bold());
                    result.addElement(LookupElementBuilder.create("return").bold());

                    for (JavaScriptKeywordCompletionExtender extender : JavaScriptKeywordCompletionExtender.EP_NAME.getExtensionList()) {
                        extender.fillCompletion(parameters, context, result);
                    }
                }
            }
        );
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
