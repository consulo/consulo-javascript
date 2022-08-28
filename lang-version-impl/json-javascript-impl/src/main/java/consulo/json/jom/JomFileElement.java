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

package consulo.json.jom;

import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import consulo.json.jom.proxy.JomProxyInvocationHandler;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.lazy.LazyValue;

import javax.annotation.Nonnull;
import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JomFileElement<T extends JomElement>
{
	private final JomFileDescriptor<T> myFileDescriptor;
	private final JSFile myPsiFile;
	private final Supplier<T> myRootValue;

	public JomFileElement(JSFile psiFile, JomFileDescriptor<T> fileDescriptor)
	{
		myPsiFile = psiFile;
		myFileDescriptor = fileDescriptor;
		myRootValue = LazyValue.notNull(() -> (T) JomProxyInvocationHandler.createProxy(myFileDescriptor.getDefinitionClass(), PsiTreeUtil.findChildOfType(myPsiFile, JSObjectLiteralExpression.class)));
	}

	@Nonnull
	public T getRootElement()
	{
		return myRootValue.get();
	}

	@Nonnull
	public JomFileDescriptor<T> getFileDescriptor()
	{
		return myFileDescriptor;
	}

	@Nonnull
	public JSFile getFile()
	{
		return myPsiFile;
	}
}
