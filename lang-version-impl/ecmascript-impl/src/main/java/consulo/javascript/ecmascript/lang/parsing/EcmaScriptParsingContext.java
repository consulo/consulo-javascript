/*
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.ecmascript.lang.parsing;

import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;
import consulo.javascript.lang.parsing.StatementParsing;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class EcmaScriptParsingContext extends JavaScriptParsingContext {
    @Nonnull
    @Override
    protected StatementParsing createStatementParsing() {
        return new EcmaScriptStatementParsing(this);
    }

    @Nonnull
    @Override
    protected ExpressionParsing createExpressionParsing() {
        return new EcmaScriptExpressionParsing(this);
    }
}
