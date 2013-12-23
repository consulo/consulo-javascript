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

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.intention.IntentionManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.jetbrains.annotations.NonNls;

import java.awt.*;
import java.util.List;
import java.util.Map;

public class JSIntentionTestManager extends TestManager {

    static final String LOG_CATEGORY = "#org.intellij.idea.lang.javascript.test.JSIntentionTestManager";

    @NonNls private static final String  INTENTION_DESCRIPTION_PATH = "/intentionDescriptions";

    private IntentionAction[] intentionActions;

    public JSIntentionTestManager(Project project) {
        super(project);
        this.intentionActions = IntentionManager.getInstance().getIntentionActions();
    }

    @Override
	protected TestCase createTestCase(TestManager manager, String familyName, String caseName,
                                      String beforeStatement, String afterStatement, boolean noDetectionExpected) {
        return new IntentionTestCase(manager, familyName, caseName, beforeStatement, afterStatement, noDetectionExpected);
    }

    public static void runTest(Project project) {
        JSIntentionTestManager       manager       = new JSIntentionTestManager(project);
        final String                 directoryPath = JSIntentionTestManager.class.getResource(INTENTION_DESCRIPTION_PATH).getPath().substring(6);
        Map<String, List<TestCase>>  testCases     = manager.getTestCases(directoryPath);
        InfoDialog                   infoDialog    = new IntentionInfoDialog(testCases.size());

        manager.addIntentions(testCases);
        for (final String testCaseFamily : testCases.keySet()) {
            for (final TestCase testCase : testCases.get(testCaseFamily)) {
                testCase.process(infoDialog);
            }
        }

        Logger.getInstance(LOG_CATEGORY).info(infoDialog.getLog());
        infoDialog.close();
        manager.release();
    }

    private void addIntentions(Map<String, List<TestCase>> testCases) {
        for (final IntentionAction action : this.intentionActions) {
            final String         actionFamilyName = action.getFamilyName();
            final List<TestCase> testCaseList     = testCases.get(actionFamilyName);

            if (testCaseList != null) {
                for (TestCase testCase : testCaseList) {
                    testCase.addRunner(action);
                }
            }
        }
    }

    private static class IntentionInfoDialog extends InfoDialog {
        public IntentionInfoDialog(int numIntentions) throws HeadlessException {
            super(numIntentions, JSIntentionBundle.message("plugin.test.title"));
        }

        @Override
		protected String getMessage(String key, Object... params) {
            return JSIntentionBundle.message(key, params);
        }
    }
}
