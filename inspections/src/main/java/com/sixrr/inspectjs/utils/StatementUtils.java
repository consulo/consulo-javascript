package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSEmptyStatement;
import com.intellij.lang.javascript.psi.JSBlockStatement;

public class StatementUtils {
    private StatementUtils() {
        super();
    }

    public static boolean isEmpty(JSStatement body) {
        if (body instanceof JSEmptyStatement) {
            return true;
        }
        else if (body instanceof JSBlockStatement block) {
            JSStatement[] statements = block.getStatements();
            return statements == null || statements.length == 0;
        }
        return false;
    }
}
