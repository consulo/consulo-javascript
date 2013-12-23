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
import java.lang.ref.WeakReference;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.extapi.psi.PsiElementBase;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.ModificationTracker;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.FileStatusManager;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlToken;
import com.intellij.util.IncorrectOperationException;

/**
 * @by Maxim.Mossienko
 */
final class MyJSNamedItem extends PsiElementBase implements JSNamedElementProxy, ModificationTracker
{
	private final int myOffset;
	private int myNameId;
	private int myFlags;
	private WeakReference<PsiElement> myCachedElement;
	private JSIndexEntry myJsIndexEntry;

	private static final int DEFINITION_TAG = 1;
	private static final int FUNCTION_TAG = 2;
	private static final int FUNCTION_EXPRESSION_TAG = 3;
	private static final int FUNCTION_PROPERTY_TAG = 4;
	private static final int PROPERTY_TAG = 5;
	private static final int VARIABLE_TAG = 6;
	private static final int CLASS_TAG = 7;
	private static final int NAMESPACE_TAG = 8;
	private static final int MEMBER_VARIABLE_TAG = 9;
	private static final int MEMBER_FUNCTION_TAG = 10;
	private static final int IMPLICIT_FUNCTION_TAG = 11;
	private static final int IMPLICIT_VARIABLE_TAG = 12;
	private static final int ATTR_VALUE_TAG = 13;
	private static final int UNDEFINED_TAG = 15;

	private static final int VALUE_TAG_MASK = 0xF;
	private static final int VALUE_AND_INLINE_ATTRS_MASK = 0xF1F;

	private static final int BROWSER_TAG_MASK = 0xE0;
	private static final int BROWSER_TAG_SHIFT = 5;
	private static final int IE_SPECIFIC_ITEM_TAG = 1;
	private static final int GECKO_SPECIFIC_ITEM_TAG = 2;
	private static final int OPERA_SPECIFIC_ITEM_TAG = 4;

	private static final int DEPRECATED_TAG_MASK = 0x1;
	private static final int DEPRECATED_TAG_SHIFT = 4;

	private static final int VISIBILITY_TAG_SHIFT = 8;
	private static final int VISIBILITY_TAG_MASK = 0x3;

	private static final int GET_PROPERTY_SHIFT = 10;
	private static final int SET_PROPERTY_SHIFT = 11;
	private static final int CONSTRUCTOR_PROPERTY_SHIFT = 12;
	private static final int OVERRIDE_PROPERTY_SHIFT = 13;
	private static final int STATIC_PROPERTY_SHIFT = 14;
	private static final int DYNAMIC_PROPERTY_SHIFT = 10; // GET_PROPERTY is set for functions
	private static final int INTERFACE_PROPERTY_SHIFT = 11;
	private static final int HAS_CONSTRUCTOR_PROPERTY_SHIFT = 12;

	// WE ARE LIMITED BY SHORT

	MyJSNamedItem(final JSIndexEntry jsIndexEntry, int _offset, int _nameId, NamedItemType _type)
	{
		myJsIndexEntry = jsIndexEntry;
		myOffset = _offset;
		myNameId = _nameId;
		final int valueTag = _type == NamedItemType.Definition ? DEFINITION_TAG : _type == NamedItemType.Function ? FUNCTION_TAG : _type == NamedItemType
				.FunctionExpression ? FUNCTION_EXPRESSION_TAG : _type == NamedItemType.FunctionProperty ? FUNCTION_PROPERTY_TAG : _type == NamedItemType
				.Property ? PROPERTY_TAG : _type == NamedItemType.Variable ? VARIABLE_TAG : _type == NamedItemType.MemberVariable ? MEMBER_VARIABLE_TAG : _type
				== NamedItemType.MemberFunction ? MEMBER_FUNCTION_TAG : _type == NamedItemType.Clazz ? CLASS_TAG : _type == NamedItemType.Namespace ?
				NAMESPACE_TAG : _type == NamedItemType.ImplicitFunction ? IMPLICIT_FUNCTION_TAG : _type == NamedItemType.ImplicitVariable ?
				IMPLICIT_VARIABLE_TAG : _type == NamedItemType.AttributeValue ? ATTR_VALUE_TAG : UNDEFINED_TAG;
		assert valueTag != UNDEFINED_TAG;
		myFlags = valueTag;
	}

	MyJSNamedItem(final JSIndexEntry jsIndexEntry, DeserializationContext context) throws IOException
	{
		myJsIndexEntry = jsIndexEntry;
		myOffset = context.inputStream.readInt();
		myNameId = context.inputStream.readInt();
		final int attributes = context.inputStream.readShort();

		myFlags = attributes & VALUE_AND_INLINE_ATTRS_MASK;

		final byte browserSpecific = (byte) ((attributes & BROWSER_TAG_MASK) >> BROWSER_TAG_SHIFT);
		if(browserSpecific == IE_SPECIFIC_ITEM_TAG)
		{
			context.browserSupportManager.addIESpecificSymbol(this);
		}
		else if(browserSpecific == GECKO_SPECIFIC_ITEM_TAG)
		{
			context.browserSupportManager.addGeckoSpecificSymbol(this);
		}
		else if(browserSpecific == OPERA_SPECIFIC_ITEM_TAG)
		{
			context.browserSupportManager.addOperaSpecificSymbol(this);
		}

		String type = context.myNames.get(context.inputStream.readInt());
		if(type.length() > 0)
		{
			context.typeEvaluateManager.setElementType(this, type);
		}
	}

	@Override
	@NotNull
	public Language getLanguage()
	{
		return JavaScriptSupportLoader.JAVASCRIPT.getLanguage();
	}

	@Override
	@NotNull
	public PsiElement[] getChildren()
	{
		return PsiElement.EMPTY_ARRAY;
	}

	@Override
	public PsiElement getParent()
	{
		return null;
	}

	@Override
	@Nullable
	public PsiElement getFirstChild()
	{
		return null;
	}

	@Override
	@Nullable
	public PsiElement getLastChild()
	{
		return null;
	}

	@Override
	@Nullable
	public PsiElement getNextSibling()
	{
		return null;
	}

	@Override
	public boolean isPhysical()
	{
		return getContainingFile().isPhysical();
	}

	@Override
	@Nullable
	public PsiElement getPrevSibling()
	{
		return null;
	}

	@Override
	public TextRange getTextRange()
	{
		return new TextRange(myOffset, myOffset + 1);
	}

	@Override
	public int getStartOffsetInParent()
	{
		return 0;
	}

	@Override
	public int getTextLength()
	{
		return 1;
	}

	@Override
	public boolean equals(final Object obj)
	{
		if(obj == this)
		{
			return true;
		}
		if(!(obj instanceof MyJSNamedItem))
		{
			return false;
		}
		final MyJSNamedItem item = (MyJSNamedItem) obj;

		return myJsIndexEntry == item.myJsIndexEntry && myNameId == item.myNameId && myOffset == item.myOffset;
	}

	@Override
	public int hashCode()
	{
		return (myNameId << 7) + myOffset;
	}

	@Override
	public PsiElement findElementAt(int offset)
	{
		return null;
	}

	@Override
	public int getTextOffset()
	{
		return myOffset;
	}

	@Override
	public String getText()
	{
		return null;
	}

	@Override
	@NotNull
	public char[] textToCharArray()
	{
		return new char[0];
	}

	@Override
	public boolean textContains(char c)
	{
		return false;
	}

	@Override
	public ASTNode getNode()
	{
		return null;
	}

	@Override
	public PsiElement setName(@NotNull String name) throws IncorrectOperationException
	{
		final PsiElement element = getElement();
		if(element instanceof PsiNamedElement)
		{
			((PsiNamedElement) element).setName(name);
		}
		myNameId = JavaScriptIndex.getInstance(getProject()).getIndexOf(name);
		return this;
	}

	@Override
	public boolean isEquivalentTo(PsiElement another)
	{
		PsiElement element = getElement();
		if(element instanceof XmlToken)
		{
			element = PsiTreeUtil.getParentOfType(element, XmlAttributeValue.class);
		}
		return Comparing.equal(element, another);
	}

	@Override
	public void write(SerializationContext context) throws IOException
	{
		context.outputStream.writeInt(myOffset);
		context.outputStream.writeInt(context.myNames.get(context.myIndex.getStringByIndex(myNameId)));

		final int valueTag = myFlags & VALUE_AND_INLINE_ATTRS_MASK;

		final int browserSpecific = (context.browserSupportManager.isGeckoSpecificSymbol(this) ? GECKO_SPECIFIC_ITEM_TAG : context.browserSupportManager
				.isIESpecificSymbol(this) ? IE_SPECIFIC_ITEM_TAG : context.browserSupportManager.isOperaSpecificSymbol(this) ? OPERA_SPECIFIC_ITEM_TAG : 0) <<
				BROWSER_TAG_SHIFT;
		context.outputStream.writeShort(valueTag | browserSpecific);

		String elementType = context.typeEvaluateManager.getElementType(this);
		if(elementType == null)
		{
			elementType = "";
		}
		context.outputStream.writeInt(context.myNames.get(elementType));
	}

	@Override
	public void enumerateNames(final SerializationContext context)
	{
		context.addName(context.myIndex.getStringByIndex(myNameId));
		final String elementType = context.typeEvaluateManager.getElementType(this);
		context.addName(elementType != null ? elementType : "");
	}

	@Override
	public String getName()
	{
		return JavaScriptIndex.getInstance(getProject()).getStringByIndex(myNameId);
	}

	@Override
	public String getQualifiedName()
	{
		JSNamespace namespace = getNamespace();
		final String classPackage = namespace != null ? namespace.getQualifiedName(JavaScriptIndex.getInstance(getProject())) : null;

		final String className = getName();
		return classPackage != null && classPackage.length() > 0 ? classPackage.concat(".").concat(className) : className;
	}

	@Override
	public PsiFile getContainingFile()
	{
		return myJsIndexEntry.getFile();
	}

	@Override
	public boolean isValid()
	{
		return getContainingFile().isValid();
	}

	@Override
	@NotNull
	public Project getProject()
	{
		return getContainingFile().getProject();
	}

	@Override
	public PsiManager getManager()
	{
		return getContainingFile().getManager();
	}

	@Override
	public PsiElement getNavigationElement()
	{
		final PsiElement element = getElement();
		if(element instanceof XmlToken)
		{
			return element.getParent();
		}
		return element;
	}

	@Override
	public PsiElement getElement()
	{
		PsiElement element = myCachedElement != null ? myCachedElement.get() : null;

		if(element == null || !element.isValid())
		{
			element = null;
			final NamedItemType valueType = getType();
			if(valueType == NamedItemType.Clazz)
			{
				element = ((JSFile) getContainingFile()).findStubbedElementAtOffset(myOffset, JSClass.class);
			}

			if(element == null)
			{
				PsiElement elementAt = getContainingFile().findElementAt(myOffset);

				if(valueType != NamedItemType.AttributeValue && elementAt != null)
				{
					element = PsiTreeUtil.getNonStrictParentOfType(elementAt, JSNamedElement.class, JSStatement.class, PsiComment.class);

					if(valueType == NamedItemType.FunctionExpression && element instanceof JSDefinitionExpression)
					{
						element = ((JSAssignmentExpression) element.getParent()).getROperand();
						if(element instanceof JSNewExpression)
						{
							element = ((JSNewExpression) element).getMethodExpression();
						}
					}
					else if(valueType == NamedItemType.FunctionProperty && element instanceof JSFunctionExpression)
					{
						element = element.getParent();
					}
				}
				else
				{
					element = elementAt;
				}
			}
			if(element == null)
			{
				element = PsiUtilBase.NULL_PSI_ELEMENT;
			}

			myCachedElement = new WeakReference<PsiElement>(element);
		}
		return element != PsiUtilBase.NULL_PSI_ELEMENT ? element : null;
	}


	public Icon getIcon(int flags)
	{
		final JSAttributeList.AccessType type = getAccessType();
		return JSElementImpl.buildIcon(getIconBase(), type.getIcon());
	}

	private Icon getIconBase()
	{
		final NamedItemType valueType = getType();

		if(valueType == NamedItemType.Function ||
				valueType == NamedItemType.FunctionExpression ||
				valueType == NamedItemType.MemberFunction ||
				valueType == NamedItemType.ImplicitFunction ||
				valueType == NamedItemType.FunctionProperty)
		{
			if(valueType == NamedItemType.MemberFunction && (hasProperty(Property.GetFunction) || hasProperty(Property.SetFunction)))
			{
				return AllIcons.Nodes.Property;
			}
			return AllIcons.Nodes.Function;
		}

		if(valueType == NamedItemType.Variable ||
				valueType == NamedItemType.MemberVariable ||
				valueType == NamedItemType.ImplicitVariable)
		{
			return AllIcons.Nodes.Variable;
		}
		if(valueType == NamedItemType.Property)
		{
			return AllIcons.Nodes.Property;
		}
		if(valueType == NamedItemType.AttributeValue)
		{
			return AllIcons.Nodes.Tag;
		}
		if(valueType == NamedItemType.Clazz || valueType == NamedItemType.Namespace)
		{
			return hasProperty(Property.Interface) ? AllIcons.Nodes.Interface : AllIcons.Nodes.Class;
		}

		return AllIcons.Nodes.Variable;
	}

	@Override
	public NamedItemType getType()
	{
		final int valueTag = myFlags & VALUE_TAG_MASK;

		final NamedItemType valueType = valueTag == DEFINITION_TAG ? NamedItemType.Definition : valueTag == FUNCTION_TAG ? NamedItemType.Function :
				valueTag == FUNCTION_EXPRESSION_TAG ? NamedItemType.FunctionExpression : valueTag == FUNCTION_PROPERTY_TAG ? NamedItemType.FunctionProperty :
						valueTag == MEMBER_FUNCTION_TAG ? NamedItemType.MemberFunction : valueTag == PROPERTY_TAG ? NamedItemType.Property : valueTag == VARIABLE_TAG
								? NamedItemType.Variable : valueTag == MEMBER_VARIABLE_TAG ? NamedItemType.MemberVariable : valueTag == CLASS_TAG ? NamedItemType.Clazz :
								valueTag == NAMESPACE_TAG ? NamedItemType.Namespace : valueTag == IMPLICIT_FUNCTION_TAG ? NamedItemType.ImplicitFunction : valueTag ==
										IMPLICIT_VARIABLE_TAG ? NamedItemType.ImplicitVariable : valueTag == ATTR_VALUE_TAG ? NamedItemType.AttributeValue : null;
		if(valueType == null)
		{
			throw new NullPointerException();
		}
		return valueType;
	}

	@Override
	@Nullable
	public ItemPresentation getPresentation()
	{
		return new JSItemPresentation(this, myJsIndexEntry.getNamespace(this));
	}

	@Override
	public JSNamespace getNamespace()
	{
		return myJsIndexEntry.getNamespace(this);
	}

	@Override
	public JSIndexEntry getEntry()
	{
		return myJsIndexEntry;
	}

	public FileStatus getFileStatus()
	{
		return FileStatusManager.getInstance(getProject()).getStatus(myJsIndexEntry.getVirtualFile());
	}

	@Override
	public void navigate(boolean requestFocus)
	{
		Navigatable navItem = getNavigatable();
		if(navItem != null)
		{
			navItem.navigate(requestFocus);
		}
	}

	@Override
	public boolean canNavigate()
	{
		Navigatable navItem = getNavigatable();
		return navItem != null && (navItem.canNavigate() || navItem instanceof XmlToken);
	}

	private Navigatable getNavigatable()
	{
		final PsiElement element = getElement();
		return element instanceof Navigatable ? (Navigatable) element : null;
	}

	@Override
	public boolean canNavigateToSource()
	{
		return canNavigate();
	}

	@Override
	public int getNameId()
	{
		return myNameId;
	}

	@Override
	public boolean isDeprecated()
	{
		return ((myFlags >> DEPRECATED_TAG_SHIFT) & DEPRECATED_TAG_MASK) != 0;
	}

	@Override
	public JSAttributeList.AccessType getAccessType()
	{
		final int i = (myFlags >> VISIBILITY_TAG_SHIFT) & VISIBILITY_TAG_MASK;
		return types[i];
	}

	@Override
	public boolean hasProperty(final Property property)
	{
		return ((myFlags >> getPropertyShift(property)) & 0x1) != 0;
	}

	private static int getPropertyShift(final Property property)
	{
		int shift = -1;
		if(property == Property.GetFunction)
		{
			shift = GET_PROPERTY_SHIFT;
		}
		else if(property == Property.SetFunction)
		{
			shift = SET_PROPERTY_SHIFT;
		}
		else if(property == Property.Constructor)
		{
			shift = CONSTRUCTOR_PROPERTY_SHIFT;
		}
		else if(property == Property.Override)
		{
			shift = OVERRIDE_PROPERTY_SHIFT;
		}
		else if(property == Property.Dynamic)
		{
			shift = DYNAMIC_PROPERTY_SHIFT;
		}
		else if(property == Property.Interface)
		{
			shift = INTERFACE_PROPERTY_SHIFT;
		}
		else if(property == Property.HasConstructor)
		{
			shift = HAS_CONSTRUCTOR_PROPERTY_SHIFT;
		}
		else if(property == Property.Static)
		{
			shift = STATIC_PROPERTY_SHIFT;
		}
		if(shift == -1)
		{
			throw new IllegalArgumentException("Illegal property passed:" + property);
		}
		return shift;
	}

	public void setProperty(final Property property, boolean value)
	{
		if(value)
		{
			myFlags |= (1 << getPropertyShift(property));
		}
		else
		{
			myFlags &= ~(1 << getPropertyShift(property));
		}
	}

	@Override
	public boolean isWritable()
	{
		return getContainingFile().isWritable();
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		getElement().delete();
	}

	@Override
	public long getModificationCount()
	{
		return myJsIndexEntry.getFile().getModificationStamp();
	}

	public void setDeprecated(final boolean deprecated)
	{
		if(deprecated)
		{
			myFlags |= (1 << DEPRECATED_TAG_SHIFT);
		}
		else
		{
			myFlags &= ~(1 << DEPRECATED_TAG_SHIFT);
		}
	}

	public void setAccessType(@NotNull JSAttributeList.AccessType accessType)
	{
		final int ord = accessType.ordinal();
		myFlags &= ~(VISIBILITY_TAG_MASK << VISIBILITY_TAG_SHIFT);
		myFlags |= (ord << VISIBILITY_TAG_SHIFT);
		assert accessType == getAccessType();
	}

	private static final JSAttributeList.AccessType[] types = JSAttributeList.AccessType.values();

	@Override
	@Nullable
	public ASTNode findNameIdentifier()
	{
		return null;
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		return null;
	}
}
