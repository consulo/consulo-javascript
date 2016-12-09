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

/*
 * @author max
 */
package consulo.javascript.psi.stubs.index;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import consulo.javascript.lang.psi.stubs.JavaScriptIndexKeys;

/**
 * @author VISTALL
 */
public class JavaScriptClassNameIndex extends StringStubIndexExtension<JSClass>
{
	@NotNull
	@Override
	public StubIndexKey<String, JSClass> getKey()
	{
		return JavaScriptIndexKeys.CLASSES_BY_NAME;
	}
}