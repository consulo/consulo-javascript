/*
 * Copyright 2000-2005 JetBrains s.r.o
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

package com.intellij.lang.javascript;

import com.intellij.lexer.DelegateLexer;
import com.intellij.lexer.Lexer;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
public class JavaScriptParsingLexer extends DelegateLexer
{
	private boolean myOnBreakOrContinueOrReturn = false;
	private boolean myOnSemanticLineFeed = false;

	private final int ON_BREAK_OR_CONTINUE_OR_RETURN;
	private final int ON_SEMANTIC_LF;

	public JavaScriptParsingLexer(Lexer delegate, int lastState)
	{
		super(delegate);
		ON_BREAK_OR_CONTINUE_OR_RETURN = lastState;
		ON_SEMANTIC_LF = ON_BREAK_OR_CONTINUE_OR_RETURN + 1;
	}

	@Override
	public void advance()
	{
		if(!myOnSemanticLineFeed)
		{
			super.advance();
			final IElementType type = getTokenType();

			if(myOnBreakOrContinueOrReturn && type == JSTokenTypes.WHITE_SPACE)
			{
				boolean hasLineFeed = false;
				for(int i = super.getTokenStart(); i < super.getTokenEnd(); i++)
				{
					if(getBufferSequence().charAt(i) == '\n')
					{
						hasLineFeed = true;
						break;
					}
				}

				if(hasLineFeed)
				{
					myOnSemanticLineFeed = true;
				}
			}

			myOnBreakOrContinueOrReturn = (type == JSTokenTypes.BREAK_KEYWORD ||
					type == JSTokenTypes.CONTINUE_KEYWORD ||
					type == JSTokenTypes.RETURN_KEYWORD);
		}
		else
		{
			myOnSemanticLineFeed = false;
			myOnBreakOrContinueOrReturn = false;
		}
	}

	@Override
	public IElementType getTokenType()
	{
		return myOnSemanticLineFeed ? JSTokenTypes.SEMANTIC_LINEFEED : super.getTokenType();
	}

	@Override
	public int getTokenStart()
	{
		return super.getTokenStart();
	}

	@Override
	public int getTokenEnd()
	{
		return myOnSemanticLineFeed ? super.getTokenStart() : super.getTokenEnd();
	}

	@Override
	public int getState()
	{
		if(myOnSemanticLineFeed)
		{
			return ON_SEMANTIC_LF;
		}
		if(myOnBreakOrContinueOrReturn)
		{
			return ON_BREAK_OR_CONTINUE_OR_RETURN;
		}
		return super.getState();
	}
}
