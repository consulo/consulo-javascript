package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.fix.RenameFix;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class ReservedWordUsedAsNameJSInspection extends JavaScriptInspection {
    @NonNls
    private static Set<String> reservedWords = new HashSet<String>();
    private static Set<String> reservedWordExclusionsForECMAScript = new HashSet<String>();

    static
    {
        reservedWords.add("abstract");
        reservedWords.add("boolean");
        reservedWords.add("break");
        reservedWords.add("byte");
        reservedWords.add("case");
        reservedWords.add("catch");
        reservedWords.add("char");
        reservedWords.add("class");
        reservedWords.add("continue");
        reservedWords.add("const");
        reservedWords.add("debugger");
        reservedWords.add("default");
        reservedWords.add("delete");
        reservedWords.add("do");
        reservedWords.add("double");
        reservedWords.add("else");
        reservedWords.add("extends");
        reservedWords.add("enum");
        reservedWords.add("export");
        reservedWords.add("false");
        reservedWords.add("final");
        reservedWords.add("finally");
        reservedWords.add("float");
        reservedWords.add("for");
        reservedWords.add("function");
        reservedWords.add("if");
        reservedWords.add("implements");
        reservedWords.add("import");
        reservedWords.add("in");
        reservedWords.add("instanceof");
        reservedWords.add("int");
        reservedWords.add("interface");
        //reservedWords.add("label"); // not a reserved word
        reservedWords.add("long");
        reservedWords.add("native");
        reservedWords.add("new");
        reservedWords.add("null");
        reservedWords.add("package");
        reservedWords.add("private");
        reservedWords.add("protected");
        reservedWords.add("public");
        reservedWords.add("return");
        reservedWords.add("short");
        reservedWords.add("static");
        reservedWords.add("super");
        reservedWords.add("switch");
        reservedWords.add("synchronized");
        reservedWords.add("this");
        reservedWords.add("throw");
        reservedWords.add("throws");
        reservedWords.add("transient");
        reservedWords.add("true");
        reservedWords.add("try");
        reservedWords.add("typeof");
        reservedWords.add("var");
        reservedWords.add("void");
        reservedWords.add("volatile");
        reservedWords.add("while");
        reservedWords.add("with");

        reservedWordExclusionsForECMAScript.add("int");
    }
    @NotNull
    public String getID() {
        return "ReservedWordAsName";
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("reserved.word.used.as.name.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("reserved.word.used.as.name.error.string");
    }

    protected InspectionJSFix buildFix(PsiElement location) {
        return new RenameFix();
    }

    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    public boolean isEnabledByDefault() {
        return  true;
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ReservedWordAsNameVisitor();
    }

    private static class ReservedWordAsNameVisitor extends BaseInspectionVisitor {

        @Override public void visitJSProperty(JSProperty jsProperty) {
            super.visitJSProperty(jsProperty);
            final String name = jsProperty.getName();
            if(isReserved(name))
            {
                registerError(jsProperty.getFirstChild());
            }
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression jsReferenceExpression) {
            super.visitJSReferenceExpression(jsReferenceExpression);
            final PsiElement nameElement = jsReferenceExpression.getReferenceNameElement();
            if(nameElement== null)
            {
                return;
            }

            final String s = nameElement.getText();
            if(isReserved(s)) {
                if (!reservedWordExclusionsForECMAScript.contains(s) ||
                    JSUtils.getDialect(jsReferenceExpression.getContainingFile()) == null) {
                    registerError(nameElement);
                }
            }
        }
    }

    private static boolean isReserved(String text) {
        return reservedWords.contains(text);
    }
}
