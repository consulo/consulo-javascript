/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

package com.intellij.lang.javascript.index;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.psi.PsiElement;

/**
 * @by Maxim.Mossienko
 */
public interface JSNamedElementProxy extends JSQualifiedNamedElement
{
	void write(SerializationContext context) throws IOException;

	void enumerateNames(final SerializationContext context);

	int getNameId();

	boolean isDeprecated();

	JSAttributeList.AccessType getAccessType();

	enum Property
	{
		GetFunction, SetFunction, Constructor, Dynamic, Override, Static, Interface, HasConstructor
	}

	boolean hasProperty(Property property);

	enum NamedItemType
	{
		Definition, Variable, Function, Property, FunctionProperty, FunctionExpression, AttributeValue, Clazz, Namespace, MemberVariable,
		MemberFunction, ImplicitFunction, ImplicitVariable
	}

	PsiElement getElement();

	NamedItemType getType();

	JSNamespace getNamespace();

	JSIndexEntry getEntry();
}
