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

package consulo.javascript.psi;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.util.PsiTreeUtil;
import org.jspecify.annotations.Nullable;

/**
 * {@code static { ... }} in a class body (ES2022) - run once when the class is evaluated
 *
 * @author VISTALL
 */
public interface JSClassStaticBlock extends JSElement {
    @Nullable
    @RequiredReadAction
    default JSBlockStatement getBlock() {
        return PsiTreeUtil.getChildOfType(this, JSBlockStatement.class);
    }
}
