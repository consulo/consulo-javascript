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
package com.intellij.lang.javascript.psi.impl;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSDocTagValue;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSType;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.util.ArrayUtil;
import com.intellij.util.text.StringTokenizer;

public class JSDocTagValueImpl extends JSElementImpl implements JSDocTagValue
{
	private volatile PsiReference[] myRefs;
	private volatile long myModificationCount = -1;

	public JSDocTagValueImpl(final ASTNode node)
	{
		super(node);
	}

	@NotNull
	@Override
	public PsiReference[] getReferences()
	{
		final long count = getManager().getModificationTracker().getModificationCount();

		if(count != myModificationCount)
		{
			final @NonNls String name = ((PsiNamedElement) getParent()).getName();
			PsiReference[] result = PsiReference.EMPTY_ARRAY;

			if(name != null)
			{
				@NonNls String text = getText();

				boolean soft = false;

				if("class".equals(name))
				{
					soft = text.indexOf('.') == -1;
				}
				else if("see".equals(name))
				{
					soft = true;
					if(!text.startsWith("http:"))
					{
						if(text.indexOf('#') != -1)
						{ //Array#length, flash.ui.ContextMenu#customItems, #updateProperties()
							soft = false;
						}
						else
						{
							int dotPos = text.lastIndexOf('.');
							if(dotPos != -1 && dotPos + 1 < text.length() && Character.isUpperCase(text.charAt(dotPos + 1)))
							{
								soft = false; // flash.ui.ContextMenu
							}
						}
					}
				}
				else if("returns".equals(name))
				{
					soft = true;
				}

				int offset = 0;
				if(text.startsWith("{"))
				{
					text = text.substring(1);
					offset++;
					soft = false;
				}
				if(text.endsWith("}"))
				{
					text = text.substring(0, text.length() - 1);
				}

				final StringTokenizer tokenizer = new StringTokenizer(text, JSType.COMMENT_DELIMITERS);
				while(tokenizer.hasMoreElements())
				{
					@NonNls String textFragment = tokenizer.nextElement();
					int localOffset = offset + tokenizer.getCurrentPosition() - textFragment.length();
					if(textFragment.endsWith("[]"))
					{
						textFragment = textFragment.substring(0, textFragment.length() - 2);
					}
					int i = textFragment.indexOf('?');
					if(i != -1)
					{
						textFragment = textFragment.substring(0, i);
					}

					if(!"void".equals(textFragment))
					{
						final JSReferenceSet set = new JSReferenceSet(this, textFragment, localOffset, soft, false, false);
						result = ArrayUtil.mergeArrays(result, set.getReferences(), PsiReference.ARRAY_FACTORY);
					}
				}
			}

			myRefs = result;
			myModificationCount = count;
		}

		return myRefs;
	}

	@Override
	public void accept(@NotNull final PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSDocTagValue(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}
