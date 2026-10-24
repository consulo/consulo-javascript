/*
 * Copyright 2013-2026 consulo.io
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
package consulo.javascript.debugger.browser;

import consulo.disposer.Disposable;
import consulo.execution.configuration.fragment.RunConfigurationFragment;
import consulo.execution.configuration.fragment.RunConfigurationFragmentEditor;
import consulo.execution.configuration.fragment.RunConfigurationFragmentLayout;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.TextBox;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.util.lang.StringUtil;

/**
 * The page a {@link JavaScriptBrowserConfiguration} opens and debugs.
 *
 * @author VISTALL
 * @since 2026-10-24
 */
class JavaScriptBrowserUrlFragment implements RunConfigurationFragment<JavaScriptBrowserConfiguration> {
    @Override
    public String getId() {
        return "browserUrl";
    }

    @Override
    public LocalizeValue getName() {
        return LocalizeValue.localizeTODO("URL");
    }

    @Override
    public boolean isRemovable() {
        return false;
    }

    @RequiredUIAccess
    @Override
    public RunConfigurationFragmentEditor<JavaScriptBrowserConfiguration> createEditor(Project project, Disposable parent) {
        TextBox urlBox = TextBox.create();

        return new RunConfigurationFragmentEditor<>() {
            @RequiredUIAccess
            @Override
            public void addRows(RunConfigurationFragmentLayout layout) {
                layout.addLabeled(LocalizeValue.localizeTODO("URL:"), urlBox);
            }

            @RequiredUIAccess
            @Override
            public void reset(JavaScriptBrowserConfiguration target) {
                urlBox.setValue(StringUtil.notNullize(target.getUrl()));
            }

            @RequiredUIAccess
            @Override
            public void apply(JavaScriptBrowserConfiguration target) {
                target.setUrl(urlBox.getValue());
            }
        };
    }
}
