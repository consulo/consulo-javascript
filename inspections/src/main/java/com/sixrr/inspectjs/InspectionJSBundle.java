package com.sixrr.inspectjs;

import com.intellij.CommonBundle;
import org.jetbrains.annotations.PropertyKey;

import java.util.ResourceBundle;

public class InspectionJSBundle {
    private static final ResourceBundle ourBundle =
            ResourceBundle.getBundle("com.sixrr.inspectjs.InspectionJSBundle");

    private InspectionJSBundle() {
    }

    public static String message(@PropertyKey(resourceBundle = "com.sixrr.inspectjs.InspectionJSBundle")String key, Object... params) {
        return CommonBundle.message(ourBundle, key, params);
    }
}
