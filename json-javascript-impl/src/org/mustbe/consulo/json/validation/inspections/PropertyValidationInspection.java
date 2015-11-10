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

package org.mustbe.consulo.json.validation.inspections;

import java.util.ArrayDeque;
import java.util.Deque;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectPropertyDescriptor;
import org.mustbe.consulo.json.validation.descriptor.JsonPropertyDescriptor;
import org.mustbe.consulo.json.validation.JsonFileDescriptorProviders;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.PairProcessor;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class PropertyValidationInspection extends LocalInspectionTool
{
	@NotNull
	@Override
	public PsiElementVisitor buildVisitor(@NotNull final ProblemsHolder holder, boolean isOnTheFly)
	{
		return new JSElementVisitor()
		{
			@Override
			@RequiredReadAction
			public void visitJSProperty(JSProperty node)
			{
				JsonObjectDescriptor rootDescriptor = JsonFileDescriptorProviders.getRootDescriptor(node.getContainingFile());
				if(rootDescriptor == null)
				{
					return;
				}

				final Deque<JSProperty> queue = new ArrayDeque<JSProperty>();
				PsiTreeUtil.treeWalkUp(node, null, new PairProcessor<PsiElement, PsiElement>()
				{
					@Override
					public boolean process(PsiElement element, PsiElement element2)
					{
						if(element instanceof JSProperty)
						{
							queue.addFirst((JSProperty) element);
						}
						return true;
					}
				});

				JsonObjectDescriptor currentObject = rootDescriptor;
				for(JSProperty property : queue)
				{
					String name = property.getName();
					if(name == null)
					{
						return;
					}

					JsonPropertyDescriptor propertyDescriptor = currentObject.getProperty(name);
					if(propertyDescriptor == null)
					{
						if(node == property)
						{
							PsiElement nameIdentifier = node.getNameIdentifier();
							assert nameIdentifier != null;

							holder.registerProblem(nameIdentifier, "Undefined property", ProblemHighlightType.ERROR);
						}
						return;
					}
					else if(propertyDescriptor instanceof JsonObjectPropertyDescriptor)
					{
						currentObject = ((JsonObjectPropertyDescriptor) propertyDescriptor).getObjectDescriptor();
					}
				}
			}
		};
	}
}
