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

package consulo.javascript.run.debug;

import java.io.File;

import org.chromium.sdk.Script;
import javax.annotation.Nonnull;
import com.intellij.openapi.fileTypes.FileTypeRegistry;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;

/**
 * @author VISTALL
 * @since 22.03.14
 */
public class V8ScriptUtil
{
	private static final Key<Object> SOME_VALUE_TO_EQUALS = Key.create("some-value-to-equals");

	@Nonnull
	public static VirtualFile toVirtualFile(@Nonnull Script value, boolean toOpen)
	{
		String scriptName = value.getName();
		assert scriptName != null;
		VirtualFile fileByPath = LocalFileSystem.getInstance().findFileByPath(scriptName);
		if(fileByPath != null)
		{
			return fileByPath;
		}

		String name = new File(scriptName).getName();
		LightVirtualFile virtualFile = new LightVirtualFile(name, FileTypeRegistry.getInstance().getFileTypeByFileName(name),
				toOpen ? value.getSource() : "")
		{
			@Override
			public boolean equals(Object obj)
			{
				if(obj instanceof LightVirtualFile)
				{
					if(!Comparing.equal(getPath(), ((LightVirtualFile) obj).getPath()))
					{
						return false;
					}
					Object userData = ((LightVirtualFile) obj).getUserData(SOME_VALUE_TO_EQUALS);
					Object userData1 = getUserData(SOME_VALUE_TO_EQUALS);
					return Comparing.equal(userData1, userData);
				}
				return false;
			}
		};
		virtualFile.putUserData(SOME_VALUE_TO_EQUALS, value);
		return virtualFile;
	}
}
