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

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.jetbrains.annotations.NonNls;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.fileTypes.ZipArchiveFileType;

public abstract class TestManager {

    @NonNls protected static final String  TEST_SUBDIRECTORY       = "test";
    @NonNls protected static final String  BEFORE_FILE_NAME_PREFIX = "before";
    @NonNls protected static final String  AFTER_FILE_NAME_PREFIX  = "after";
    @NonNls protected static final String  NONE_FILE_NAME_PREFIX   = "none";
    @NonNls protected static final String  JS_EXTENSION            = ".js";

    @NonNls
    protected static final CharsetDecoder DECODER     = Charset.forName(System.getProperty("file.encoding")).newDecoder();
    protected static final byte[]         BUFFER      = new byte[1024];
    protected static final ByteBuffer     BYTE_BUFFER = ByteBuffer.allocate(BUFFER.length);

    protected Project project;
    protected Editor  editor;

    public TestManager(Project project) {
        final EditorFactory editorFactory = EditorFactory.getInstance();
        final Document      document      = editorFactory.createDocument("");

        this.project = project;
        this.editor  = editorFactory.createEditor(document, project);
    }

    public void release() {
        EditorFactory.getInstance().releaseEditor(this.editor);
    }

    public Project getProject() {
        return this.project;
    }

    public Editor getEditor() {
        return this.editor;
    }

    protected abstract TestCase createTestCase(TestManager manager,
                                               String      familyName,
                                               String      caseName,
                                               String      beforeStatement,
                                               String      afterStatement,
                                               boolean     noDetectionExpected);

    protected Map<String, List<TestCase>> getTestCases(String directoryPath) {
        Map<String, List<TestCase>> caseMap         = new LinkedHashMap<String, List<TestCase>>();
        final VirtualFile           directory       = ZipArchiveFileType.INSTANCE.getFileSystem().findFileByPath(directoryPath);
        final VirtualFile[]         caseDirectories = ((directory == null) ? null : directory.getChildren());

        if (caseDirectories != null) {
            for (final VirtualFile caseDirectory : caseDirectories) {
                final VirtualFile[] caseFiles = caseDirectory.getChildren();

                for (final VirtualFile file : caseFiles) {
                    final String caseFamilyName = caseDirectory.getName();

                    if (file.isDirectory() && file.getName().equals(TEST_SUBDIRECTORY)) {
                        final VirtualFile[] testFiles = file.getChildren();

                        for (final VirtualFile subFile : testFiles) {
                            this.addTestCase(caseMap, subFile, testFiles, caseFamilyName);
                        }
                    } else {
                        this.addTestCase(caseMap, file, caseFiles, caseFamilyName);
                    }
                }
            }
        }
        return caseMap;
    }

    protected void addTestCase(Map<String, List<TestCase>> caseMap, VirtualFile file,
                               VirtualFile[] caseFiles, String caseFamilyName) {
        final String fileName = file.getName();

        if (fileName.startsWith(BEFORE_FILE_NAME_PREFIX)) {
            final String      fileSuffix = fileName.substring(BEFORE_FILE_NAME_PREFIX.length());
            final VirtualFile afterFile  = getAfterFile(caseFiles, AFTER_FILE_NAME_PREFIX + fileSuffix);

            this.addCase(caseMap, caseFamilyName, fileSuffix, file, afterFile, false);
        } else if (fileName.startsWith(NONE_FILE_NAME_PREFIX)) {
            final String  fileSuffix = fileName.substring(NONE_FILE_NAME_PREFIX.length());

            this.addCase(caseMap, caseFamilyName, fileSuffix, file, null, true);
        }
    }

    private void addCase(Map<String, List<TestCase>> caseMap, String caseFamilyName, String fileSuffix,
                         VirtualFile file, VirtualFile afterFile, boolean detectionExpected) {
        String caseName = (fileSuffix.startsWith(JS_EXTENSION) ? fileSuffix.substring(JS_EXTENSION.length()) :
                           fileSuffix.endsWith  (JS_EXTENSION) ? fileSuffix.substring(0, fileSuffix.length() - JS_EXTENSION.length())
                                                               : fileSuffix);

        if (caseName.charAt(0) == '.') {
            caseName = caseName.substring(1);
        }
        if (caseName.charAt(caseName.length() - 1) == '.') {
            caseName = caseName.substring(0, caseName.length() - 1);
        }

        List<TestCase> caseList = caseMap.get(caseFamilyName);

        if (caseList == null) {
            caseList = new ArrayList<TestCase>(2);
            caseMap.put(caseFamilyName, caseList);
        }
        caseList.add(this.createTestCase(this, caseFamilyName, caseName,
                                         getFileContents(file), getFileContents(afterFile),
                                         detectionExpected));
    }

    protected static VirtualFile getAfterFile(VirtualFile[] fileList, String fileName) {
        for (final VirtualFile file : fileList) {
            if (file.getName().equals(fileName)) {
                return file;
            }
        }
        return null;
    }

    protected static String getFileContents(VirtualFile file) {
        if (file != null) {
            try {
                final StringBuilder builder     = new StringBuilder((int) file.getLength());
                final InputStream   inputStream = file.getInputStream();

                while (inputStream.available() > 0) {
                    final int numReadBytes = inputStream.read(BUFFER);

                    BYTE_BUFFER.clear();
                    BYTE_BUFFER.put(BUFFER, 0, numReadBytes);
                    BYTE_BUFFER.flip();
                    builder.append(DECODER.decode(BYTE_BUFFER));
                }
                inputStream.close();

                return builder.toString();
            } catch (IOException e) {
                // Default processing: return an empty string
            }
        }

        return "";
    }
}
