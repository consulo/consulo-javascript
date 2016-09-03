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

package consulo.javascript.lang.psi.stubs;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.psi.stubs.StubIndexKey;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public interface JavaScriptIndexKeys
{
	StubIndexKey<String, JSClass> CLASSES_BY_NAME = StubIndexKey.createIndexKey("js.class.shortName");
	StubIndexKey<String, JSQualifiedNamedElement> ELEMENTS_BY_NAME = StubIndexKey.createIndexKey("js.qualified.shortName");
	StubIndexKey<String, JSQualifiedNamedElement> ELEMENTS_BY_QNAME = StubIndexKey.createIndexKey("js.element.qualifiedName");

	StubIndexKey<String, JSReferenceList> EXTENDS_INDEX = StubIndexKey.createIndexKey("JS.class.super");
	StubIndexKey<String, JSReferenceList> IMPLEMENTED_INDEX = StubIndexKey.createIndexKey("JS.class.implements");
}
