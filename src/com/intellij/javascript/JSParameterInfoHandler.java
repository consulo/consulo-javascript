package com.intellij.javascript;

import com.intellij.codeInsight.CodeInsightBundle;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupValueWithPsiElement;
import com.intellij.codeInsight.lookup.MutableLookupElement;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.parameterInfo.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.searches.DefinitionsSearch;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ArrayUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * @author Maxim.Mossienko
 */
public class JSParameterInfoHandler implements ParameterInfoHandlerWithTabActionSupport<JSArgumentList, JSFunction,JSExpression> {
  private static final Set<Class> ourArgumentListAllowedParentClassesSet = new HashSet<Class>(Arrays.asList(JSCallExpression.class));

  public boolean couldShowInLookup() {
    return true;
  }

  public Object[] getParametersForLookup(final LookupElement item, final ParameterInfoContext context) {
    if (!(item instanceof MutableLookupElement)) return null;

    final Object o = item.getObject();

    if (o instanceof LookupValueWithPsiElement) {
      PsiElement element = ((LookupValueWithPsiElement)o).getElement();
      if (element instanceof JSNamedElementProxy) element = ((JSNamedElementProxy)element).getElement();

      if (element instanceof JSFunction) {
        final JSFunction originalFunction = (JSFunction)element;
        final List<JSFunction> lookupItems = new ArrayList<JSFunction>();
        Set<String> availableSignatures = new HashSet<String>();

        for(PsiElement el : DefinitionsSearch.search(originalFunction)) {
          doAddSignature(lookupItems, availableSignatures, el);
        }

        if (lookupItems.size() == 0) lookupItems.add(originalFunction);

        return lookupItems.toArray(new Object[lookupItems.size()]);
      }
    }

    return ArrayUtil.EMPTY_OBJECT_ARRAY;
  }

  private static void doAddSignature(final List<JSFunction> lookupItems, final Set<String> availableSignatures, final PsiElement el) {
    if (el instanceof JSFunction) {
      final JSFunction function = (JSFunction)el;
      final JSParameterList parameterList = function.getParameterList();

      if (parameterList != null) {
        final String typedSignature = buildSignature(parameterList.getParameters(), false, -1).text;
        final String untypedSignature = buildSignature(parameterList.getParameters(), true, -1).text;

        if (!availableSignatures.contains(typedSignature) && !availableSignatures.contains(untypedSignature)) {
          lookupItems.add(function);
          availableSignatures.add(typedSignature);
          availableSignatures.add(untypedSignature);
        }
      }
    }
  }

  public Object[] getParametersForDocumentation(final JSFunction p, final ParameterInfoContext context) {
    final JSParameterList list = p.getParameterList();
    if (list != null) return list.getParameters();
    return ArrayUtil.EMPTY_OBJECT_ARRAY;
  }

  public JSArgumentList findElementForParameterInfo(final CreateParameterInfoContext context) {
    JSArgumentList argList = findArgumentList(context.getFile(), context.getOffset());

    if (argList != null) {
      return fillSignaturesForArgumentList(context, argList);
    }
    return argList;
  }

  @Nullable
  public static JSArgumentList findArgumentList(final PsiFile file, final int offset) {
    JSArgumentList argList = ParameterInfoUtils.findParentOfType(file, offset, JSArgumentList.class);
    if (argList == null) {
      final JSCallExpression callExpression =
        ParameterInfoUtils.findParentOfType(file, offset, JSCallExpression.class);
      if (callExpression != null) {
        argList = callExpression.getArgumentList();
      }
    }
    return argList;
  }

  @Nullable
  private static JSArgumentList fillSignaturesForArgumentList(final CreateParameterInfoContext context, final @NotNull JSArgumentList argList) {
    final PsiElement psiElement = argList.getParent();
    if (!(psiElement instanceof JSCallExpression)) return null;

    final JSCallExpression parent = (JSCallExpression)psiElement;
    final JSExpression methodExpression = parent.getMethodExpression();

    if (methodExpression instanceof JSReferenceExpression) {
      final ResolveResult[] resolveResults = ((JSReferenceExpression)methodExpression).multiResolve(true);

      if (resolveResults.length > 0) {
        List<JSFunction> items = new ArrayList<JSFunction>(resolveResults.length);
        Set<String> availableSignatures = new HashSet<String>();

        for(ResolveResult r:resolveResults) {
          PsiElement element = r.getElement();

          if (element instanceof JSNamedElementProxy) element = ((JSNamedElementProxy)element).getElement();

          if (element instanceof JSProperty) {
            element = ((JSProperty)element).getValue();
          }

          doAddSignature(items, availableSignatures, element);
        }

        context.setItemsToShow(ArrayUtil.toObjectArray(items));
        return argList;
      }
    } else if (methodExpression instanceof JSSuperExpression) {
      final PsiElement clazz = methodExpression.getReference().resolve();
      if (clazz instanceof JSFunction) {
        context.setItemsToShow(new Object[] {clazz});
        return argList;
      }
    }
    return null;
  }

  public void showParameterInfo(@NotNull final JSArgumentList element, final CreateParameterInfoContext context) {
    context.showHint(element,element.getTextOffset(), this);
  }

  public JSArgumentList findElementForUpdatingParameterInfo(final UpdateParameterInfoContext context) {
    return findArgumentList(context.getFile(), context.getOffset());
  }

  public void updateParameterInfo(@NotNull final JSArgumentList o, final UpdateParameterInfoContext context) {
    if (context.getParameterOwner() != o) {
      context.removeHint();
      return;
    }
    final int currentParameterIndex = ParameterInfoUtils.getCurrentParameterIndex(o.getNode(), context.getOffset(), JSTokenTypes.COMMA);
    context.setCurrentParameter(currentParameterIndex);
  }

  @NotNull
  public String getParameterCloseChars() {
    return ",){";
  }

  public boolean tracksParameterIndex() {
    return true;
  }

  public void updateUI(final JSFunction p, final ParameterInfoUIContext context) {
    final JSParameterList parameterList = p.getParameterList();
    final JSParameter[] params = parameterList != null ? parameterList.getParameters():new JSParameter[0];
    final int currentParameterIndex = context.getCurrentParameterIndex() >= 0 ? context.getCurrentParameterIndex():params.length;
    final JSParameter parameter = currentParameterIndex < params.length ? params[currentParameterIndex]:null;

    final SignatureInfo signatureInfo = buildSignature(params, false, currentParameterIndex);
    final String name = signatureInfo.text;

    final String currentParameterSignature = parameter != null ?getSignatureForParameter(parameter, false):null;
    int highlightStart = parameter != null ? signatureInfo.selectedParameterStart:0;
    int highlightEnd = parameter != null ? highlightStart + currentParameterSignature.length() : 0;
    context.setupUIComponentPresentation(name,highlightStart,highlightEnd,false,false,false,context.getDefaultParameterColor());
  }

  private static class SignatureInfo {
    String text;
    int selectedParameterStart = -1;
  }

  private static @NotNull SignatureInfo buildSignature(final JSParameter[] params, final boolean skipType, int selectedParameterIndex) {
    SignatureInfo info = new SignatureInfo();
    if (params.length > 0) {
      StringBuilder result = new StringBuilder();
      for(int i = 0; i < params.length; ++i) {
        if (result.length() > 0) result.append(", ");
        if (selectedParameterIndex == i) info.selectedParameterStart = result.length();
        result.append(getSignatureForParameter(params[i], skipType));
      }

      info.text = result.toString();
    } else {
      info.text = CodeInsightBundle.message("parameter.info.no.parameters");
    }
    return info;
  }

  public static String getSignatureForParameter(final JSParameter p, boolean skipType) {
    final String s = skipType ? null: p.getTypeString();

    if (s != null && s.length() > 0) {
      final boolean ecmal4 = p.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
      String result;

      if (ecmal4) {
        if (p.isRest()) {
          result = "...";
        } else {
          result = p.getName() + ":" + s;
        }
      }
      else result = "[" + s + "] " + p.getName();
      final String initializerText = p.getInitializerText();
      if (initializerText != null) result += " = " + initializerText;
      return result;
    }
    return p.getName();
  }

  @NotNull
  public JSExpression[] getActualParameters(@NotNull final JSArgumentList jsArgumentList) {
    return jsArgumentList.getArguments();
  }

  @NotNull
  public IElementType getActualParameterDelimiterType() {
    return JSTokenTypes.COMMA;
  }

  @NotNull
  public IElementType getActualParametersRBraceType() {
    return JSTokenTypes.RBRACE;
  }

  @NotNull
  public Set<Class> getArgumentListAllowedParentClasses() {
    return ourArgumentListAllowedParentClassesSet;
  }

  @NotNull
  public Class<JSArgumentList> getArgumentListClass() {
    return JSArgumentList.class;
  }
}
