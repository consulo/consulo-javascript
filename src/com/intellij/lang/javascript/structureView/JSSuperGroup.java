package com.intellij.lang.javascript.structureView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.Icon;

import org.jetbrains.annotations.Nullable;
import com.intellij.ide.util.treeView.smartTree.Group;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.util.IconLoader;

/**
 * @author Maxim.Mossienko
 */
public class JSSuperGroup implements Group, ItemPresentation
{
	private final String myName;
	private final List<TreeElement> myChildren;
	private static final Icon myIcon = IconLoader.getIcon("/general/inheritedMethod.png");

	public JSSuperGroup(final String name)
	{
		myName = name;
		myChildren = new ArrayList<TreeElement>();
	}

	@Override
	public ItemPresentation getPresentation()
	{
		return this;
	}

	@Override
	public Collection<TreeElement> getChildren()
	{
		return myChildren;
	}

	@Override
	public String getPresentableText()
	{
		return myName;
	}

	@Override
	@Nullable
	public String getLocationString()
	{
		return null;
	}

	@Override
	@Nullable
	public Icon getIcon(final boolean open)
	{
		return myIcon;
	}

	@Nullable
	public TextAttributesKey getTextAttributesKey()
	{
		return null;
	}

	void addChild(TreeElement element)
	{
		myChildren.add(element);
	}
}
