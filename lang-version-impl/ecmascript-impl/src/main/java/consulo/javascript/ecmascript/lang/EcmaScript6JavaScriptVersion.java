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

package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.lang.psi.impl.resolve.JavaScriptVersionWithHelper;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.StandardJavaScriptVersion;
import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
@ExtensionImpl
public class EcmaScript6JavaScriptVersion extends BaseEcmaScriptJavaScriptVersion implements StandardJavaScriptVersion, JavaScriptVersionWithHelper
{
	@Nonnull
	public static EcmaScript6JavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(EcmaScript6JavaScriptVersion.class);
	}

	@Inject
	public EcmaScript6JavaScriptVersion()
	{
		super("ECMASCRIPT_6");
	}

	public EcmaScript6JavaScriptVersion(@Nonnull String id)
	{
		super(id);

		addFeature(JavaScriptFeature.CLASS);
		addFeature(JavaScriptFeature.BINARY_LITERAL);
		addFeature(JavaScriptFeature.OCTAL_LITERAL);
		addFeature(JavaScriptFeature.PARAMETER_DEFAULT_VALUE);
		addFeature(JavaScriptFeature.REST_PARAMETER);
		addFeature(JavaScriptFeature.FUNCTION_PROPERTY);
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 6";
	}

	@Override
	public int getWeight()
	{
		return 600;
	}
}
