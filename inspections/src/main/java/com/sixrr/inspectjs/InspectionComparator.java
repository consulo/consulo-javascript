package com.sixrr.inspectjs;

import consulo.language.editor.inspection.LocalInspectionTool;
import consulo.localize.LocalizeManager;
import consulo.localize.LocalizeValue;

import java.util.Comparator;
import java.util.function.BiFunction;

class InspectionComparator implements Comparator<Class<? extends LocalInspectionTool>> {
    private static final BiFunction<LocalizeManager, String, String> UPPERCASE_STRIP_QUOTES =
        (localizeManager, string) -> stripQuotes(string.toUpperCase());

    @Override
    public int compare(Class<? extends LocalInspectionTool> class1, Class<? extends LocalInspectionTool> class2) {
        LocalInspectionTool inspection1;
        LocalInspectionTool inspection2;
        try {
            inspection1 = class1.newInstance();
            inspection2 = class2.newInstance();
        }
        catch (InstantiationException ignore) {
            return -1;
        }
        catch (IllegalAccessException ignore) {
            return -1;
        }
        LocalizeValue groupName1 = inspection1.getGroupDisplayName();
        LocalizeValue groupName2 = inspection2.getGroupDisplayName();
        int groupNameComparison = groupName1.compareTo(groupName2);
        if (groupNameComparison != 0) {
            return groupNameComparison;
        }
        LocalizeValue displayName1 = inspection1.getDisplayName().map(UPPERCASE_STRIP_QUOTES);
        LocalizeValue displayName2 = inspection2.getDisplayName().map(UPPERCASE_STRIP_QUOTES);

        return displayName1.compareTo(displayName2);
    }

    private static String stripQuotes(String str) {
        if (str.indexOf((int)'\'') < 0 && str.indexOf((int)'"') < 0) {
            return str;
        }
        int length = str.length();
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            char ch = str.charAt(i);
            if (ch != '"' && ch != '\'') {
                sb.append(ch);
            }
        }
        return sb.toString();
    }
}
