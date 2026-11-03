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
import consulo.language.psi.PsiElement;
import consulo.markdown.documentation.MarkdownDocumentationProvider;

import java.util.List;

/**
 * JSDoc and TSDoc comments as Markdown documentation - their descriptions are Markdown, as in TSDoc and in the JSDoc of most projects.
 */
@ExtensionImpl
public class JSDocMarkdownDocumentationProvider implements MarkdownDocumentationProvider {
    // "*/" before "*", so the closing line is not left with a "/"
    private static final List<String> LINE_PREFIXES = List.of("/**", "*/", "*");

    @Override
    public Class<? extends PsiElement> getCommentClass() {
        return JSDocComment.class;
    }

    @Override
    public List<String> getLinePrefixes() {
        return LINE_PREFIXES;
    }
}
