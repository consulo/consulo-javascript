/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention.test;

import org.jetbrains.annotations.NonNls;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;

/**
 * A intention test case for the JSIntentionPowerPack plugin
 */
class IntentionTestCase extends TestCase {

    private JSIntentionTestManager manager;

    public IntentionTestCase(TestManager manager,
                             String      familyName,
                             String      caseName,
                             String      beforeStatement,
                             String      afterStatement,
                             boolean     noDetectionExpected) {
        super(familyName, caseName, beforeStatement, afterStatement, noDetectionExpected);

        if (!(manager instanceof JSIntentionTestManager)) {
            throw new IllegalArgumentException();
        }

        this.manager = (JSIntentionTestManager) manager;
    }

    public void process(InfoDialog infoDialog) {
        infoDialog.setProcessedCaseName(this.familyName);

        final Project  project = this.manager.getProject();
        final Editor   editor  = this.manager.getEditor();
        String         processedStatement;
        String         suggestionText = EMPTY;

        try {
            final PsiFile psiFile = this.createCaseBeforeStatement(project, editor);

            for (final Object runner : this.runners) {
                final IntentionAction action           = (IntentionAction) runner;
                final String          actionFamilyName = action.getFamilyName();

                if (actionFamilyName != null && actionFamilyName.equals(this.familyName) &&
                    action.isAvailable(project, editor, psiFile)) {
                    suggestionText = action.getText();
                    action.invoke(project, editor, psiFile);
                }
            }

            processedStatement = psiFile.getText();
        } catch (Throwable e) {
            processedStatement = this.beforeStatement;
            e.printStackTrace();
        }

        @NonNls final String  state;
        final boolean         detectionOk = (this.noDetectionExpected ^ (suggestionText.length() != 0));
        final boolean         fixOk       = (this.afterStatement.length() == 0 ||
                                             compareIgnoreBlanks(processedStatement, this.afterStatement));

        if (this.noDetectionExpected) {
            state = (detectionOk ? "OK (no detection)" : "*** KO *** (unexpected detection)");
        } else {
            state = (!detectionOk                                                 ? "*** KO *** (no detection)" :
                     this.afterStatement.length() == 0                            ? "OK (detection)" :
                     compareIgnoreBlanks(processedStatement, this.afterStatement) ? "OK (detection and fix)"
                                                                                  : "*** KO *** (fix failure => " + processedStatement + ')');
        }

        final String result = this.familyName + ' ' + this.caseName +
                              (suggestionText.length() == 0 ? "" : " (" + suggestionText + ')') +
                              ":\t" + state;

        infoDialog.addCompletedCase(detectionOk, fixOk, result);
//        final boolean        ok     = compareIgnoreBlanks(processedStatement, this.afterStatement);
//        @NonNls final String result = this.familyName + ' ' + this.caseName + " (" +
//                                      suggestionText + "):\t" + (ok ? "OK" : "*** KO ***");
//
//        infoDialog.addCompletedIntention(ok, true, result);
    }
}
