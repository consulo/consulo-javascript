/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript;

import consulo.language.Language;
import consulo.language.file.FileTypeManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.util.collection.ArrayUtil;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.ide.highlighter.XmlFileType;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;
import org.jetbrains.annotations.NonNls;

/**
 * @by max, maxim.mossienko
 */
@Deprecated
public class JavaScriptSupportLoader
{
	@Deprecated
	public static final Language ECMA_SCRIPT_L4 = new Language("ECMA4_DEPRECATED") {};


	public static final
	@NonNls
	String MXML_FILE_EXTENSION_DOT = ".mxml";
	public static final
	@NonNls
	String MXML_FILE_EXTENSION2_DOT = ".mxm";
	public static final
	@NonNls
	String MXML_URI = "http://www.adobe.com/2006/mxml";
	public static final
	@NonNls
	String MXML_URI2 = "http://www.macromedia.com/2003/mxml";
	public static final
	@NonNls
	String MXML_URI3 = "http://ns.adobe.com/mxml/2009";
	public static final
	@NonNls
	String MXML_URI4 = "library://ns.adobe.com/flex/spark";
	public static final
	@NonNls
	String MXML_URI5 = "library://ns.adobe.com/flex/halo";
	public static final
	@NonNls
	String MXML_URI6 = "http://ns.adobe.com/fxg/2008";
	public static final
	@NonNls
	String[] MXML_URIS = {
			MXML_URI,
			MXML_URI2,
			MXML_URI3,
			MXML_URI4,
			MXML_URI5,
			MXML_URI6
	};
	public static final
	@NonNls
	String BINDOWS_URI = "http://www.bindows.net";
	@NonNls
	public static final String ACTION_SCRIPT_CLASS_TEMPLATE_NAME = "ActionScript Class";
	@NonNls
	public static final String ACTION_SCRIPT_INTERFACE_TEMPLATE_NAME = "ActionScript Interface";
	@NonNls
	public static final String MXML_COMPONENT_TEMPLATE_NAME = "Mxml Component";

	public static boolean isFlexMxmFile(final PsiFile file)
	{
		return file.getFileType() == XmlFileType.INSTANCE && nameHasMxmlExtension(file.getName());
	}

	public static boolean isFlexMxmFile(final VirtualFile file)
	{
		return file.getFileType() == XmlFileType.INSTANCE && nameHasMxmlExtension(file.getName());
	}

	private static boolean nameHasMxmlExtension(final String s)
	{
		return s.endsWith(MXML_FILE_EXTENSION_DOT) || s.endsWith(MXML_FILE_EXTENSION2_DOT);
	}

	public static boolean isFlexMxmFile(String filename)
	{
		return FileTypeManager.getInstance().getFileTypeByFileName(filename) == XmlFileType.INSTANCE && nameHasMxmlExtension(filename);
	}

	public static boolean isBindowsFile(final PsiElement element)
	{
		final PsiFile containingFile = element.getContainingFile();
		final PsiElement tag = element.getParent().getParent();
		if(!(tag instanceof XmlTag))
		{
			return false;
		}
		if(BINDOWS_URI.equals(((XmlTag) tag).getNamespace()))
		{
			return true;
		}
		if(!(containingFile instanceof XmlFile))
		{
			return false;
		}

		return "Application".equals(((XmlFile) containingFile).getDocument().getRootTag().getName());
	}

	public static boolean isMxmlNs(final String ns)
	{
		return ArrayUtil.contains(ns, MXML_URIS);
	}

}
