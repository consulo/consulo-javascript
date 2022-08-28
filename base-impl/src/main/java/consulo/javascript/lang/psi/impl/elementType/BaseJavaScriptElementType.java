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

package consulo.javascript.lang.psi.impl.elementType;

import org.jetbrains.annotations.NonNls;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSStubElement;
import consulo.javascript.language.psi.JavaScriptTypeElement;

/**
 * @author VISTALL
 * @since 01.03.2016
 */
public abstract class BaseJavaScriptElementType<S extends JSStubElement<P>, P extends JavaScriptTypeElement> extends JSStubElementType<S, P>
{
	public BaseJavaScriptElementType(@NonNls String debugName)
	{
		super(debugName);
	}
}
