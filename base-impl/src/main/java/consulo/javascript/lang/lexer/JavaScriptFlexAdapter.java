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

package consulo.javascript.lang.lexer;

import consulo.language.lexer.FlexAdapter;
import consulo.language.lexer.FlexLexer;

/**
 * @author Maxim.Mossienko
 */
public class JavaScriptFlexAdapter extends FlexAdapter {
    private static final int BASE_STATE_MASK = 0xF;
    private static final int TAG_COUNT_SHIFT = 4;

    public JavaScriptFlexAdapter(FlexLexer lexer) {
        super(lexer);
    }

    @Override
    public void start(CharSequence buffer, int startOffset, int endOffset, int initialState) {
        super.start(buffer, startOffset, endOffset, initialState & BASE_STATE_MASK);
        ((JavaScriptFlexLexer)getFlex()).setTagCount(initialState >> TAG_COUNT_SHIFT);
    }

    @Override
    public int getState() {
        return getStateInternal() + (((JavaScriptFlexLexer)getFlex()).getTagCount() << TAG_COUNT_SHIFT);
    }

    protected int getStateInternal() {
        return super.getState();
    }
}