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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import org.jetbrains.annotations.NonNls;

import java.math.BigInteger;
import java.util.regex.Pattern;

/**
 *
 */
public class NumberUtil
{
	private NumberUtil()
	{
	}

	@NonNls
	public static final Pattern decimalPattern = Pattern.compile("[0-9]+");
	@NonNls
	public static final Pattern octalPattern = Pattern.compile("0[0-7]+");
	@NonNls
	public static final Pattern hexPattern = Pattern.compile("0[xX][0-9a-fA-F]+");

	public static BigInteger getLiteralNumber(JSLiteralExpression expression)
	{
		String expressionText = expression.getText();
		int radix = 10;

		if(expressionText == null || expressionText.length() == 0)
		{
			return new BigInteger(new byte[]{0});
		}

		if(expressionText.charAt(0) == '0')
		{
			if(expressionText.length() > 2 && Character.toUpperCase(expressionText.charAt(1)) == 'X')
			{
				expressionText = expressionText.substring(2);
				radix = 16;
			}
			else if(octalPattern.matcher(expressionText).matches())
			{
				radix = 8;
			}
		}

		return new BigInteger(expressionText, radix);
	}

	public static boolean isOctal(String number)
	{
		return octalPattern.matcher(number).matches();
	}

	public static boolean isDecimal(String number)
	{
		return decimalPattern.matcher(number).matches();
	}

	public static boolean isHex(String number)
	{
		return hexPattern.matcher(number).matches();
	}
}
