package com.intellij.lang.javascript.navigation;

import com.intellij.codeInsight.navigation.actions.TypeDeclarationProvider;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JSNamespace;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.index.JavaScriptSymbolProcessor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.BaseJSSymbolProcessor;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:27:42 PM
 */
public class JavaScriptTypeDeclarationProvider implements TypeDeclarationProvider {
  public PsiElement[] getSymbolTypeDeclarations(final PsiElement symbol) {
    if (!(symbol instanceof JSNamedElement)) {
      return null;
    }
    String s = null;
    final PsiFile containingFile = symbol.getContainingFile();
    if (symbol instanceof JSFunction) {
      s = ((JSFunction)symbol).getReturnTypeString();
    }
    else if (symbol instanceof JSVariable) {
      s = ((JSVariable)symbol).getTypeString();
    }
    else if (symbol instanceof JSExpression) {
      final BaseJSSymbolProcessor.SimpleTypeProcessor processor =
          new BaseJSSymbolProcessor.SimpleTypeProcessor(containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4);
      BaseJSSymbolProcessor.doEvalForExpr((JSExpression)symbol, containingFile, processor);
      s = processor.getType();
    }

    // TODO: BaseJSSymbolProcessor.doEvalForExpr does all above !
    if (s != null) {
      final JavaScriptIndex index = JavaScriptIndex.getInstance(symbol.getProject());
      PsiElement item = JSResolveUtil.findClassByQName(s, symbol);

      if (item == null) {
        final String s1 = s;
        final List<PsiElement> result = new ArrayList<PsiElement>();

        index.processAllSymbols(new JavaScriptSymbolProcessor.DefaultSymbolProcessor() {
          protected boolean process(final PsiElement namedElement, final JSNamespace namespace) {
            if (namedElement.isPhysical()) {
              result.add(0, namedElement);
            }
            else {
              result.add(namedElement);
            }
            return true;
          }

          public PsiFile getBaseFile() {
            return containingFile;
          }

          public int getRequiredNameId() {
            return index.getIndexOf(s1);
          }
        });
        if (!result.isEmpty()) {
          item = result.get(0);
        }
      }

      if (item instanceof JSNamedElementProxy) {
        item = ((JSNamedElementProxy)item).getElement();
      }
      if (item != null && item.isPhysical()) return new PsiElement[]{item};
    }

    return new PsiElement[]{symbol};
  }
}
