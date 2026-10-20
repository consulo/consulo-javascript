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
package consulo.javascript.impl.ignore;

import consulo.annotation.component.ExtensionImpl;
import consulo.virtualFileSystem.fileType.FileNameMatcherFactory;
import consulo.virtualFileSystem.fileType.FileTypeConsumer;
import consulo.virtualFileSystem.fileType.FileTypeFactory;
import jakarta.inject.Inject;

/**
 * @author VISTALL
 * @since 2026-10-20
 */
@ExtensionImpl
public class JavaScriptIgnoreFileTypeFactory extends FileTypeFactory {
    private final FileNameMatcherFactory myFileNameMatcherFactory;

    @Inject
    public JavaScriptIgnoreFileTypeFactory(FileNameMatcherFactory fileNameMatcherFactory) {
        myFileNameMatcherFactory = fileNameMatcherFactory;
    }

    @Override
    public void createFileTypes(FileTypeConsumer consumer) {
        consumer.consume(
            ESLintIgnoreFileType.INSTANCE,
            myFileNameMatcherFactory.createExactFileNameMatcher(ESLintIgnoreLanguage.INSTANCE.getFilename())
        );
        consumer.consume(ESLintIgnoreFileType.INSTANCE, ESLintIgnoreLanguage.INSTANCE.getExtension());
        consumer.consume(
            PrettierIgnoreFileType.INSTANCE,
            myFileNameMatcherFactory.createExactFileNameMatcher(PrettierIgnoreLanguage.INSTANCE.getFilename())
        );
        consumer.consume(PrettierIgnoreFileType.INSTANCE, PrettierIgnoreLanguage.INSTANCE.getExtension());
        consumer.consume(
            JSHintIgnoreFileType.INSTANCE,
            myFileNameMatcherFactory.createExactFileNameMatcher(JSHintIgnoreLanguage.INSTANCE.getFilename())
        );
        consumer.consume(JSHintIgnoreFileType.INSTANCE, JSHintIgnoreLanguage.INSTANCE.getExtension());
        consumer.consume(
            NuxtIgnoreFileType.INSTANCE,
            myFileNameMatcherFactory.createExactFileNameMatcher(NuxtIgnoreLanguage.INSTANCE.getFilename())
        );
        consumer.consume(NuxtIgnoreFileType.INSTANCE, NuxtIgnoreLanguage.INSTANCE.getExtension());
    }
}
