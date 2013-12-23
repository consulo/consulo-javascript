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
package org.intellij.idea.lang.javascript.intention;

import org.intellij.idea.lang.javascript.intention.test.JSIntentionTestManager;
import org.jetbrains.annotations.NotNull;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.project.Project;

public class JSIntentionPowerPack implements ProjectComponent {
    /**
     * To be set to true if you want to run plugin unit tests (both intention
     * actions and inspections) when opening an IntelliJ IDEA project.
     * Test output is printed out on the IDEA log.
     */
    public static final boolean RUN_TEST = false;

    protected final Project project;

    public JSIntentionPowerPack(Project project) {
        this.project = project;
    }

    @Override
	@NotNull public String getComponentName() {
        return JSIntentionBundle.message("plugin.JSIntentionPowerPack.name");
    }

    @Override
	public void projectOpened() {
        //noinspection ConstantConditions
        if (RUN_TEST) {
            JSIntentionTestManager.runTest(project);
        }
    }

    @Override
	public void projectClosed() {}

    @Override
	public void initComponent() {}

    @Override
	public void disposeComponent() {}
}
