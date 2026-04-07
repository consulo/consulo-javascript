/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.language;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ServiceAPI;
import consulo.application.Application;

import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * @author VISTALL
 * @since 2015-12-11
 */
@ServiceAPI(ComponentScope.APPLICATION)
public abstract class StandardJavaScriptVersions {

    public static StandardJavaScriptVersions getInstance() {
        return Application.get().getService(StandardJavaScriptVersions.class);
    }

    public abstract JavaScriptLanguageVersion getDefaultVersion();

    public abstract List<JavaScriptLanguageVersion> getValidLanguageVersions();

    public abstract JavaScriptLanguageVersion findVersionById(@Nullable String id);
}
