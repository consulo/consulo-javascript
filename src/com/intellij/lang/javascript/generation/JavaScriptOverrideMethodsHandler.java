/*
 * @author max
 */
package com.intellij.lang.javascript.generation;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.validation.ImplementedMethodProcessor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.util.Function;

import java.util.Collection;
import java.util.Map;

public class JavaScriptOverrideMethodsHandler extends BaseJSGenerateHandler {
  protected String getTitleKey() {
    return "methods.to.override.chooser.title";
  }

  protected BaseCreateMethodsFix createFix(final JSClass clazz) {
    return new OverrideMethodsFix(clazz);
  }

  protected void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates) {
    Map<String, Object> _functionsToOverride = null;
    final Function<JSFunction, Boolean> functionFilter = new Function<JSFunction, Boolean>() {
      public Boolean fun(final JSFunction function) {
        final JSAttributeList attributeList = function.getAttributeList();

        if (attributeList != null && (attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) || attributeList.hasModifier(
            JSAttributeList.ModifierType.FINAL))) {
          return Boolean.FALSE;
        }
        return Boolean.TRUE;
      }
    };

    for(JSClass superClazz: clazz.getSuperClasses()) {
      _functionsToOverride = ImplementedMethodProcessor.collectAllVisibleClassFunctions(
          superClazz,
          _functionsToOverride, functionFilter
      );
    }

    final Map<String, Object> functionsToOverride = _functionsToOverride;
    final ResolveProcessor collectOwnFunctions = new ResolveProcessor(null, clazz) {
      {
        setToProcessMembers(true);
        setToProcessHierarchy(false);
      }

      @Override
      public boolean execute(final PsiElement element, final ResolveState state) {
        if (element instanceof JSFunction) {
          final JSFunction function = (JSFunction)element;
          if (function.isConstructor() || functionsToOverride == null) return true;
          final String funName = function.getName();
          final Object o = functionsToOverride.get(funName);
          if (o instanceof JSFunction && ((JSFunction)o).getKind() == function.getKind()) functionsToOverride.remove(funName);
          else if (o instanceof JSFunction[]) {
            JSFunction[] functions = (JSFunction[])o;
            functionsToOverride.put(funName, functions[0].getKind() == function.getKind() ? functions[1]:functions[0]);
          }
        }
        return true;
      }
    };

    clazz.processDeclarations(collectOwnFunctions, ResolveState.initial(), clazz, clazz);

    if (functionsToOverride != null) {
      for(Map.Entry<String,Object> entry:functionsToOverride.entrySet()) {
        final Object value = entry.getValue();
        if (value instanceof JSFunction[]) {
          for(JSFunction function:(JSFunction[])value) {
            candidates.add(new JSNamedElementNode(function));
          }
        } else {
          candidates.add(new JSNamedElementNode((JSFunction)value));
        }
      }
    }
  }
}