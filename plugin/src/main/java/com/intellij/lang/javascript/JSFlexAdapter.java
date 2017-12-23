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

package com.intellij.lang.javascript;

import com.intellij.lexer.FlexAdapter;

/**
 * @author Maxim.Mossienko
 */
public class JSFlexAdapter extends FlexAdapter
{
	private static final int BASE_STATE_MASK = 0xF;
	private static final int TAG_COUNT_SHIFT = 4;

	public JSFlexAdapter(boolean highlight, DialectOptionHolder optionHolder)
	{
		super(new _JavaScriptLexer(highlight, optionHolder));
	}

	@Override
	public void start(final CharSequence buffer, final int startOffset, final int endOffset, final int initialState)
	{
		super.start(buffer, startOffset, endOffset, initialState & BASE_STATE_MASK);
		((_JavaScriptLexer) getFlex()).setTagCount(initialState >> TAG_COUNT_SHIFT);
	}

	@Override
	public int getState()
	{
		return getStateInternal() + (((_JavaScriptLexer) getFlex()).getTagCount() << TAG_COUNT_SHIFT);
	}

	protected int getStateInternal()
	{
		return super.getState();
	}
}