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

import java.util.ArrayList;
import java.util.List;

import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;

/**
 */
public abstract class TestCase {
    @NonNls protected static final String SPOT_START = "<spot>";
    @NonNls protected static final String SPOT_END   = "</spot>";
    @NonNls protected static final String EMPTY      = "";

    protected String       familyName;
    protected String       caseName;
    protected String       beforeStatement;
    protected String       afterStatement;
    protected boolean      noDetectionExpected;
    protected List<Object> runners;            // inspections/intentions related to the case

    public TestCase(String  familyName,
                    String  caseName,
                    String  beforeStatement,
                    String  afterStatement,
                    boolean noDetectionExpected) {
        this.familyName          = familyName;
        this.caseName            = caseName;
        this.beforeStatement     = beforeStatement;
        this.afterStatement      = afterStatement;
        this.noDetectionExpected = noDetectionExpected;
        this.runners             = new ArrayList<Object>(2);
    }

    protected PsiFile createCaseBeforeStatement(final Project project, final Editor editor) {
        final String             text        = this.beforeStatement.replace("\r\n", "\n");
        final String             jsElement   = text.replace(SPOT_START, EMPTY)
                                                   .replace(SPOT_END, EMPTY);
        final ASTNode            elementNode = JSElementFactory.createElementFromText(project, jsElement);
        final CaseDocumentSetter setter      = new CaseDocumentSetter(editor, jsElement, text.indexOf(SPOT_START));

        CommandProcessor.getInstance().executeCommand(project, new WriteActionRunner(setter), EMPTY, EMPTY);
        return elementNode.getPsi().getContainingFile();
    }

    public void addRunner(Object runner) {
        this.runners.add(runner);
    }

    public abstract void process(InfoDialog infoDialog);

    protected static boolean compareIgnoreBlanks(String s1, String s2) {
        int  len1 = s1.length();
        int  len2 = s2.length();
        int  i    = 0;
        int  j    = 0;
        char prev = 0;

        while (len1 != 0 && len2 != 0) {
            char c1 = s1.charAt(i++);
            char c2 = s2.charAt(j++);

            len1--;
            len2--;

            final boolean isPrevCharIdentifierPart = Character.isJavaIdentifierPart(prev);

            if ((Character.isWhitespace(c1) && !(Character.isJavaIdentifierPart(c2) && isPrevCharIdentifierPart)) ||
                (Character.isWhitespace(c2) && !(Character.isJavaIdentifierPart(c1) && isPrevCharIdentifierPart))) {
                while (len1 != 0 && Character.isWhitespace(c1)) {
                    c1 = s1.charAt(i++);
                    len1--;
                }
                while (len2 != 0 && Character.isWhitespace(c2)) {
                    c2 = s2.charAt(j++);
                    len2--;
                }
            }
            if (c1 != c2) {
                return false;
            }

            prev = c1;
        }

        while (len1 != 0) {
            char c1 = s1.charAt(i++);
            len1--;
            if (!Character.isWhitespace(c1)) { break; }
        }
        while (len2 != 0) {
            char c2 = s2.charAt(j++);
            len2--;
            if (!Character.isWhitespace(c2)) { break; }
        }

        return (len1 == len2);
    }

    protected static class CaseDocumentSetter implements Runnable {
        private final String jsElement;
        private final Editor editor;
        private final int    offset;

        public CaseDocumentSetter(Editor editor, String jsElement, int offset) {
            this.jsElement = jsElement;
            this.editor    = editor;
            this.offset    = offset;
        }

        @Override
		public void run() {
            final Document document = this.editor.getDocument();

            document.deleteString(0, document.getTextLength());
            document.insertString(0, this.jsElement);
            if (this.offset >= 0) {
                this.editor.getCaretModel().moveToOffset(this.offset);
            }
        }
    }

    protected static class WriteActionRunner implements Runnable {
        private final Runnable runnable;

        public WriteActionRunner(Runnable runnable) {
            this.runnable = runnable;
        }

        @Override
		public void run() {
            ApplicationManager.getApplication().runWriteAction(this.runnable);
        }
    }
}
