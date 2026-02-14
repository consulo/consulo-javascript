/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.flex;

import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.util.collection.ArrayUtil;

import jakarta.annotation.Nonnull;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 */
public class FlexImportSupport {
    // flex2.compiler.mxml.lang.StandardDefs
    private static final String[] STANDARD_MXML_IMPORTS = new String[]{
        "mx.styles.*",
        "mx.binding.*",
        "mx.core.mx_internal",
        "mx.core.IDeferredInstance",
        "mx.core.IFactory",
        "mx.core.IPropertyChangeNotifier",
        "mx.core.ClassFactory",
        "mx.core.DeferredInstanceFromClass",
        "mx.core.DeferredInstanceFromFunction",
    };

    private static final String[] AIR_ONLY_IMPLICIT_IMPORTS = new String[]{
        "flash.data.*",
        "flash.desktop.*",
        "flash.filesystem.*",
        "flash.html.*",
        "flash.html.script.*"
    };

    private static final String[] IMPLICIT_IMPORTS = new String[]{
        "flash.accessibility.*",
        "flash.debugger.*",
        "flash.display.*",
        "flash.errors.*",
        "flash.events.*",
        "flash.external.*",
        "flash.filters.*",
        "flash.geom.*",
        "flash.media.*",
        "flash.net.*",
        "flash.printing.*",
        "flash.profiler.*",
        "flash.system.*",
        "flash.text.*",
        "flash.ui.*",
        "flash.utils.*",
        "flash.xml.*"
    };

    private static final Map<String, Object> IMPLICIT_IMPORT_LIST_MAP = new HashMap<>();
    private static final Map<String, Object> MXML_IMPORT_LIST_MAP = new HashMap<>();

    static {
        fillMapFromImportsArray(IMPLICIT_IMPORTS, IMPLICIT_IMPORT_LIST_MAP);
        fillMapFromImportsArray(STANDARD_MXML_IMPORTS, MXML_IMPORT_LIST_MAP);
    }

    @RequiredReadAction
    private static void fillMapFromImportsArray(String[] strings, Map<String, Object> map) {
        for (String s : strings) {
            doAppendToMap(map, s);
        }
    }

    @RequiredReadAction
    public static void appendToMap(Map<String, Object> map, JSImportStatement importStatement) {
        doAppendToMap(map, importStatement);
    }

    @RequiredReadAction
    private static void doAppendToMap(Map<String, Object> map, Object stringOrStatement) {
        String s = stringOrStatement instanceof String string ? string : ((JSImportStatement)stringOrStatement).getImportText();

        int index = s.lastIndexOf('.');
        if (index == -1) {
            return; // nothing to import, global symbol
        }
        String key = s.substring(index + 1);
        Object o = map.get(key);

        if (o == null) {
            map.put(key, stringOrStatement);
        }
        else if (o instanceof Object[] objects) {
            map.put(key, ArrayUtil.append(objects, stringOrStatement));
        }
        else {
            map.put(key, new Object[]{stringOrStatement, o});
        }
    }

    public static JSImportedElementResolveResult resolveTypeNameUsingImplicitImports(@Nonnull String referenceName, @Nonnull JSFile file) {
        PsiElement context = file.getContext();

        if (context != null) {
            JSImportedElementResolveResult expression = tryFindInMap(referenceName, file, IMPLICIT_IMPORT_LIST_MAP, null);
            if (expression != null) {
                return expression;
            }
            expression = tryFindInMap(referenceName, file, MXML_IMPORT_LIST_MAP, null);
            if (expression != null) {
                return expression;
            }
        }
        return null;
    }

    public static JSImportedElementResolveResult tryFindInMap(
        String referenceName,
        PsiNamedElement file,
        Map<String, Object> map,
        String qName
    ) {
        JSImportedElementResolveResult resolveResult = tryEntry(map.get(referenceName), referenceName, file, qName);
        if (resolveResult == null) {
            resolveResult = tryEntry(map.get("*"), referenceName, file, qName);
        }
        return resolveResult;
    }

    public static JSImportedElementResolveResult tryFindInMap(
        String referenceName,
        PsiNamedElement file,
        Map<String, Object> map
    ) {
        return tryFindInMap(referenceName, file, map, null);
    }

    private static JSImportedElementResolveResult tryEntry(Object entry, String referenceName, PsiNamedElement file, String qName) {
        if (entry == null) {
            return null;
        }
        else if (entry instanceof Object[] entryItems) {
            for (Object entryItem : entryItems) {
                JSImportedElementResolveResult expression = tryFindClass(referenceName, file, entryItem, qName);
                if (expression != null) {
                    return expression;
                }
            }
            return null;
        }
        else {
            return tryFindClass(referenceName, file, entry, qName);
        }
    }

    @RequiredReadAction
    private static JSImportedElementResolveResult tryFindClass(
        String referenceName,
        PsiNamedElement file,
        Object entry,
        String qName
    ) {
        String importString = entry instanceof String string ? string : ((JSImportStatement)entry).getImportText();

        String nameToTry = importString.endsWith(".*")
            ? importString.substring(0, importString.length() - 1) + referenceName
            : importString;
        if (qName != null && !qName.equals(nameToTry)) {
            return null;
        }

        PsiElement element = JSResolveUtil.findClassByQName(nameToTry, file);

        if (element != null) {
            return new JSImportedElementResolveResult(
                nameToTry,
                element,
                entry instanceof JSImportStatement importStatement ? importStatement : null
            );
        }

        return null;
    }
}
