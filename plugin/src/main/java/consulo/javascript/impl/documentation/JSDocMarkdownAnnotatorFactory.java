/*
 * Copyright 2013-2026 consulo.io
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
package consulo.javascript.impl.documentation;

import com.intellij.lang.javascript.psi.JSDocComment;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.annotation.Annotator;
import consulo.language.editor.annotation.AnnotatorFactory;
import consulo.markdown.documentation.MarkdownDocumentationHighlighter;
import org.jspecify.annotations.Nullable;

/**
 * Highlights the Markdown of JSDoc and TSDoc comments, see {@link JSDocMarkdownDocumentationProvider}.
 */
@ExtensionImpl
public class JSDocMarkdownAnnotatorFactory implements AnnotatorFactory {
    @Override
    public @Nullable Annotator createAnnotator() {
        return (element, holder) -> {
            if (element instanceof JSDocComment) {
                MarkdownDocumentationHighlighter.highlight(element, holder);
            }
        };
    }

    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
