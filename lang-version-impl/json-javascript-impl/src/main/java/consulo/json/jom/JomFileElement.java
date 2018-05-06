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

import javax.annotation.Nonnull;
import consulo.json.jom.proxy.JomProxyInvocationHandler;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.openapi.util.NotNullLazyValue;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JomFileElement<T extends JomElement>
{
	private final JomFileDescriptor<T> myFileDescriptor;
	private final JSFile myPsiFile;
	private final NotNullLazyValue<T> myRootValue = new NotNullLazyValue<T>()
	{
		@Nonnull
		@Override
		protected T compute()
		{
			//noinspection unchecked
			return (T) JomProxyInvocationHandler.createProxy(myFileDescriptor.getDefinitionClass(), PsiTreeUtil.findChildOfType(myPsiFile, JSObjectLiteralExpression.class));
		}
	};

	public JomFileElement(JSFile psiFile, JomFileDescriptor<T> fileDescriptor)
	{
		myPsiFile = psiFile;
		myFileDescriptor = fileDescriptor;
	}

	@Nonnull
	public T getRootElement()
	{
		return myRootValue.getValue();
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
