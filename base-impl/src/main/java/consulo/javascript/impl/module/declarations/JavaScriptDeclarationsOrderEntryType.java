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
package consulo.javascript.impl.module.declarations;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.module.declarations.JavaScriptDeclarations;
import consulo.javascript.module.declarations.JavaScriptDeclarationsOrderEntryModel;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.content.layer.orderEntry.CustomOrderEntryTypeProvider;
import consulo.util.xml.serializer.InvalidDataException;
import org.jdom.Element;

import java.util.ArrayList;
import java.util.List;

/**
 * Stores the declarations entries: {@code <orderEntry type="javascript-declarations" name="..."><root url="..."/></orderEntry>}.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class JavaScriptDeclarationsOrderEntryType implements CustomOrderEntryTypeProvider<JavaScriptDeclarationsOrderEntryModel> {
    private static final String NAME_ATTRIBUTE = "name";
    private static final String ROOT_ELEMENT = "root";
    private static final String URL_ATTRIBUTE = "url";

    @Override
    public String getId() {
        return JavaScriptDeclarations.TYPE_ID;
    }

    @Override
    public JavaScriptDeclarationsOrderEntryModel loadOrderEntry(Element element, ModuleRootLayer moduleRootLayer) throws InvalidDataException {
        String name = element.getAttributeValue(NAME_ATTRIBUTE);
        if (name == null) {
            throw new InvalidDataException("JavaScript declarations without a name");
        }
        List<String> urls = new ArrayList<>();
        for (Element root : element.getChildren(ROOT_ELEMENT)) {
            String url = root.getAttributeValue(URL_ATTRIBUTE);
            if (url != null) {
                urls.add(url);
            }
        }
        return new JavaScriptDeclarationsOrderEntryModel(name, urls);
    }

    @Override
    public void storeOrderEntry(Element element, JavaScriptDeclarationsOrderEntryModel model) {
        element.setAttribute(NAME_ATTRIBUTE, model.getName());
        for (String url : model.getRootUrls()) {
            element.addContent(new Element(ROOT_ELEMENT).setAttribute(URL_ATTRIBUTE, url));
        }
    }
}
