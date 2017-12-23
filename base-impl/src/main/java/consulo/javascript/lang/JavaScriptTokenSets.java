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

package consulo.javascript.lang;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.TokenSet;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public interface JavaScriptTokenSets extends JSTokenTypes
{
	TokenSet STRING_LITERALS = TokenSet.create(JSTokenTypes.STRING_LITERAL, JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JSTokenTypes.INTERPOLATION_STRING_LITERAL);

	TokenSet WHITE_SPACES = TokenSet.create(JSTokenTypes.WHITE_SPACE);

	TokenSet NAME_TOKEN_TYPES = TokenSet.create(JSElementTypes.REFERENCE_EXPRESSION, JSTokenTypes.IDENTIFIER);
}
