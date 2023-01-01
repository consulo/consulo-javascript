/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention.number;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;

class ShiftUtils
{
	private ShiftUtils()
	{
	}

	public static boolean isPowerOfTwo(JSExpression expression)
	{
		if(!(expression instanceof JSLiteralExpression))
		{
			return false;
		}
		final String value = expression.getText();
		long intValue;

		try
		{
			intValue = Integer.decode(value).longValue();
		}
		catch(NumberFormatException e)
		{
			return false;
		}

		if(intValue <= 0)
		{
			return false;
		}
		while((intValue & 1) == 0)
		{
			intValue >>= 1;
		}
		return (intValue == 1);
	}

	public static int getLogBase2(JSExpression rhs)
	{
		final String value = rhs.getText();
		long intValue;

		try
		{
			intValue = Integer.decode(value).longValue();
		}
		catch(NumberFormatException e)
		{
			assert (false);
			return 0;
		}

		int log = 0;
		while((intValue & 1) == 0)
		{
			intValue >>= 1;
			log++;
		}
		return log;
	}

	public static int getExpBase2(JSExpression rhs)
	{
		final String value = rhs.getText();
		final long intValue;

		try
		{
			intValue = Integer.decode(value).longValue();
		}
		catch(NumberFormatException e)
		{
			assert (false);
			return 0;
		}

		int exp = 1;
		for(int i = 0; i < intValue; i++)
		{
			exp <<= 1;
		}
		return exp;
	}

	public static boolean isIntLiteral(JSExpression expression)
	{
		if(!(expression instanceof JSLiteralExpression))
		{
			return false;
		}

		if(!(expression instanceof JSLiteralExpression))
		{
			return false;
		}
		final String value = expression.getText();

		try
		{
			Integer.decode(value);
			return true;
		}
		catch(NumberFormatException e)
		{
			return false;
		}
	}
}
