package com.intellij.lang.javascript.structureView;

import com.intellij.ide.util.treeView.smartTree.Group;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author Maxim.Mossienko
 */
public class JSSuperGroup implements Group, ItemPresentation {
  private final String myName;
  private final List<TreeElement> myChildren;
  private static final Icon myIcon = IconLoader.getIcon("/general/inheritedMethod.png");

  public JSSuperGroup(final String name) {
    myName = name;
    myChildren = new ArrayList<TreeElement>();
  }

  public ItemPresentation getPresentation() {
    return this;
  }

  public Collection<TreeElement> getChildren() {
    return myChildren;
  }

  public String getPresentableText() {
    return myName;
  }

  @Nullable
  public String getLocationString() {
    return null;
  }

  @Nullable
  public Icon getIcon(final boolean open) {
    return myIcon;
  }

  @Nullable
  public TextAttributesKey getTextAttributesKey() {
    return null;
  }

  void addChild(TreeElement element) {
    myChildren.add(element);
  }
}
