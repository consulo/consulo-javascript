/*
 * Copyright 2000-2005 JetBrains s.r.o
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
package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.annotation.access.RequiredReadAction;
import consulo.index.io.StringRef;
import consulo.language.psi.stub.*;
import consulo.javascript.index.JavaScriptIndexer;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.javascript.module.resolve.JSModuleExport;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.javascript.impl.language.psi.stub.JSFileStubImpl;
import consulo.language.Language;
import consulo.language.psi.PsiFile;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author peter
 */
public class JSFileElementType extends IStubFileElementType<JSFileStub> {
    public JSFileElementType(Language language) {
        super(language);
    }

    @Override
    public void indexStub(JSFileStub stub, IndexSink sink) {
        JavaScriptIndexer.EP_NAME.forEachExtensionSafe(it -> it.indexFile(stub, sink));
    }

    @Override
    public StubBuilder getBuilder() {
        return new DefaultStubBuilder() {
            @Override
            @RequiredReadAction
            protected StubElement createStubForFile(PsiFile file) {
                return file instanceof JSFile jsFile
                    ? new JSFileStubImpl(jsFile, StringRef.fromString(file.getName()), buildExports(jsFile))
                    : super.createStubForFile(file);
            }
        };
    }

    /**
     * The exports of an ES module, kept in the stub of the file: an import resolves without the tree of the module
     */
    @RequiredReadAction
    protected List<JSModuleExport> buildExports(JSFile file) {
        return ResolveHelper.find(file).buildExports(file);
    }

    /**
     * The name and the exports of the file: a subclass writes its own data after them
     */
    @Override
    public void serialize(JSFileStub stub, StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
        List<JSModuleExport> exports = stub.getExports();
        dataStream.writeVarInt(exports.size());
        for (JSModuleExport export : exports) {
            export.serialize(dataStream);
        }
    }

    @Override
    public JSFileStub deserialize(StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef name = dataStream.readName();
        return new JSFileStubImpl(null, name, readExports(dataStream));
    }

    /**
     * Reads the exports written by {@link #serialize}, after the name
     */
    protected static List<JSModuleExport> readExports(StubInputStream dataStream) throws IOException {
        int count = dataStream.readVarInt();
        List<JSModuleExport> exports = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            exports.add(JSModuleExport.deserialize(dataStream));
        }
        return List.copyOf(exports);
    }

    @Override
    public String getExternalId() {
        return getLanguage() + ":" + toString();
    }

    /**
     * 49: the exports of an ES module in the file stub
     */
    @Override
    public int getStubVersion() {
        int[] version = new int[]{49};
        JavaScriptIndexer.EP_NAME.forEachExtensionSafe(javaScriptIndexer -> version[0]++);
        return version[0];
    }
}
