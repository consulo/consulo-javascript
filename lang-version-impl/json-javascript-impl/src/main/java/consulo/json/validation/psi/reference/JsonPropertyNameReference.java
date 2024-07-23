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

package consulo.json.validation.psi.reference;

import consulo.annotation.access.RequiredReadAction;
import consulo.json.validation.descriptor.JsonPropertyDescriptor;
import consulo.json.validation.inspections.PropertyValidationInspection;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.impl.reference.JSPropertyNameReference;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 02.12.2015
 */
public class JsonPropertyNameReference extends JSPropertyNameReference {
    private JsonPropertyDescriptor myPropertyDescriptor;

    public JsonPropertyNameReference(JSProperty property, PsiElement nameIdentifier, JsonPropertyDescriptor propertyDescriptor) {
        super(property, nameIdentifier);
        myPropertyDescriptor = propertyDescriptor;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement resolve() {
        PsiElement navigationElement = myPropertyDescriptor.getNavigationElement();
        if (navigationElement != null) {
            return navigationElement;
        }
        return super.resolve();
    }

    @RequiredReadAction
    @Override
    public boolean isReferenceTo(PsiElement element) {
        if (element instanceof JSProperty property) {
            JsonPropertyDescriptor propertyDescriptor = PropertyValidationInspection.findPropertyDescriptor(property);
            return myPropertyDescriptor == propertyDescriptor;
        }
        else if (element == myPropertyDescriptor.getNavigationElement()) {
            return true;
        }
        return false;
    }
}
