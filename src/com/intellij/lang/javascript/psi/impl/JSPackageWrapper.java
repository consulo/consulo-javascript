package com.intellij.lang.javascript.psi.impl;

import java.io.IOException;

import javax.swing.Icon;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.extapi.psi.PsiElementBase;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.JSPackageIndex;
import com.intellij.lang.javascript.index.JSPackageIndexInfo;
import com.intellij.lang.javascript.psi.JSPackage;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiDirectoryContainer;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
* User: Maxim.Mossienko
* Date: 19.03.2009
* Time: 16:15:13
* To change this template use File | Settings | File Templates.
*/
public class JSPackageWrapper extends PsiElementBase implements JSPackage {
  private final String name;
  private final Project project;
  private final GlobalSearchScope scope;

  public JSPackageWrapper(String _name, Project _project, GlobalSearchScope _scope) {
    name = _name;
    project = _project;
    scope = _scope;
  }

  public String getName() {
    return name.substring(name.lastIndexOf('.') + 1);
  }

  public String getQualifiedName() {
    return name;
  }

  public PsiElement setName(@NonNls @NotNull final String newName) throws IncorrectOperationException {
    final Ref<IOException> exception = new Ref<IOException>();

    int index = name.lastIndexOf('.');
    JSPackageIndex.processElementsInScope(index == -1 ? "":name.substring(0, index), name.substring(index + 1), new JSPackageIndex.PackageElementsProcessor() {
      public boolean process(VirtualFile file, String name, JSPackageIndexInfo.Kind kind) {
        if (kind != JSPackageIndexInfo.Kind.PACKAGE) return true;
        String expectedPackageNameFromFile = JSResolveUtil.getExpectedPackageNameFromFile(file, project, false);

        if (expectedPackageNameFromFile == null || expectedPackageNameFromFile.length() < name.length()) {
          return true; // optimization
        }

        while(expectedPackageNameFromFile != null && !expectedPackageNameFromFile.equals(name)) {
          file = file.getParent();
          expectedPackageNameFromFile = JSResolveUtil.getExpectedPackageNameFromFile(file, project, false);
          if (file == null) expectedPackageNameFromFile = null;
        }

        if (expectedPackageNameFromFile != null) {
          try {
            JSPsiImplUtils.doRenameParentDirectoryIfNeeded(file, newName, this);
          } catch (IOException ex) {
            exception.set(ex);
          }
        }

        return true;
      }
    }, scope, project);

    if (exception.get() != null) {
      throw new IncorrectOperationException("rename of js package failed",exception.get());
    }

    String newPackageName = JSPackageIndex.buildQualifiedName(this.name.substring(this.name.lastIndexOf('.') + 1), newName);
    return new JSPackageWrapper(newPackageName, project, scope);
  }

  public PsiElement getNameIdentifier() {
    return null;
  }

  @NotNull
  public Language getLanguage() {
    return JavaScriptSupportLoader.JAVASCRIPT.getLanguage();
  }

  @NotNull
  public PsiElement[] getChildren() {
    return PsiElement.EMPTY_ARRAY;
  }

  public PsiElement getParent() {
    //int dotPos = name.lastIndexOf('.');
    //if (dotPos == -1) return null;
    //return new JSPackageWrapper(name.substring(0, dotPos), project, scope);
    return null;
  }

  public PsiFile getContainingFile() {
    return null;
  }

  public PsiElement getFirstChild() {
    return null;
  }

  public boolean isValid() {
    return true;
  }

  public PsiElement getLastChild() {
    return null;
  }

  public PsiElement getNextSibling() {
    return null;
  }

  public PsiElement getPrevSibling() {
    return null;
  }

  public TextRange getTextRange() {
    return null;
  }

  public int getStartOffsetInParent() {
    return 0;
  }

  @NotNull
  public Project getProject() {
    return project;
  }

  public int getTextLength() {
    return 0;
  }

  public PsiElement findElementAt(final int offset) {
    return null;
  }

  public int getTextOffset() {
    return 0;
  }

  public String getText() {
    return null;
  }

  @NotNull
  public char[] textToCharArray() {
    return new char[0];
  }

  public boolean textContains(final char c) {
    return false;
  }

  public ASTNode getNode() {
    return null;
  }

  public ASTNode findNameIdentifier() {
    return null;
  }

  @NotNull
  @Override
  public GlobalSearchScope getResolveScope() {
    return scope;
  }

  @Override
  public boolean processDeclarations(@NotNull final PsiScopeProcessor processor,
                                     @NotNull final ResolveState state,
                                     PsiElement lastParent,
                                     @NotNull final PsiElement place) {
    String name = ((ResolveProcessor)processor).getName();
    boolean b =
      JSPackageIndex.processElementsInScope(this.name, name, new JSPackageIndex.PackageElementsProcessor() {
        public boolean process(VirtualFile file, String name, JSPackageIndexInfo.Kind kind) {
          String qName = JSPackageIndex.buildQualifiedName(JSPackageWrapper.this.name, name);
          PsiElement clazz = kind != JSPackageIndexInfo.Kind.PACKAGE ? JSResolveUtil.findClassByQName(qName, place) : null;
          return processor.execute(clazz != null ? clazz : new JSPackageWrapper(qName, project, scope), state);
        }
      }, scope, project);
    if (b && name != null) {
      PsiElement byQName = JSResolveUtil.findClassByQName(JSPackageIndex.buildQualifiedName(this.name, name), scope, project);
      if (byQName != null) return processor.execute(byQName, ResolveState.initial());
    }
    return b;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    JSPackageWrapper that = (JSPackageWrapper)o;

    if (name != null ? !name.equals(that.name) : that.name != null) return false;
    if (project != null ? !project.equals(that.project) : that.project != null) return false;
    if (scope != null ? !scope.equals(that.scope) : that.scope != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = name != null ? name.hashCode() : 0;
    result = 31 * result + (project != null ? project.hashCode() : 0);
    result = 31 * result + (scope != null ? scope.hashCode() : 0);
    return result;
  }

  @Override
  public PsiManager getManager() {
    return PsiManager.getInstance(project);
  }

  @Override
  public boolean isEquivalentTo(PsiElement another) {
    return isPackageReferenceOfSomeForm(name, project, scope, another);
  }

  static boolean isPackageReferenceOfSomeForm(String name, Project project, GlobalSearchScope scope, PsiElement another) {
    if (name == null) name = "";
    if (another instanceof JSPackageWrapper) return name.equals(((JSPackageWrapper)another).name);
    if (another instanceof JSPackageStatement) return name.equals(((JSPackageStatement)another).getQualifiedName());
    if (another instanceof PsiDirectoryContainer) {
      for(PsiDirectory dir:((PsiDirectoryContainer)another).getDirectories(scope)) {
        if (name.equals(JSResolveUtil.getExpectedPackageNameFromFile(dir.getVirtualFile(), project, false))) {
          return true;
        }
      }
    }
    return false;
  }

  @Override
  public boolean isWritable() {
    return true;
  }

  public Icon getIcon(int flags) {
    return AllIcons.Nodes.Package;
  }
}
