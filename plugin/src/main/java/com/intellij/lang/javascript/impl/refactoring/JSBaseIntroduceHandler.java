/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript.impl.refactoring;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.codeEditor.Editor;
import consulo.codeEditor.EditorColors;
import consulo.codeEditor.markup.RangeHighlighter;
import consulo.colorScheme.EditorColorsManager;
import consulo.colorScheme.TextAttributes;
import consulo.dataContext.DataContext;
import consulo.document.RangeMarker;
import consulo.document.util.TextRange;
import consulo.language.editor.PsiEquivalenceUtil;
import consulo.language.editor.highlight.HighlightManager;
import consulo.language.editor.refactoring.action.RefactoringActionHandler;
import consulo.language.editor.refactoring.localize.RefactoringLocalize;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.project.ui.wm.WindowManager;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.undoRedo.CommandProcessor;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author ven
 */
public abstract class JSBaseIntroduceHandler<T extends JSElement, S extends BaseIntroduceSettings, D extends JSBaseIntroduceDialog>
    implements RefactoringActionHandler {

    protected static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.refactoring.JSBaseIntroduceHandler");

    @RequiredReadAction
    protected static JSExpression findExpressionInRange(PsiFile file, int startOffset, int endOffset) {
        PsiElement element1 = file.findElementAt(startOffset);
        PsiElement element2 = file.findElementAt(endOffset - 1);
        if (element1 instanceof PsiWhiteSpace whiteSpace) {
            startOffset = whiteSpace.getTextRange().getEndOffset();
        }
        if (element2 instanceof PsiWhiteSpace whiteSpace) {
            endOffset = whiteSpace.getTextRange().getStartOffset();
        }

        JSExpression expression = PsiTreeUtil.findElementOfClassAtRange(file, startOffset, endOffset, JSExpression.class);
        int expressionEnd;
        PsiElement sibling;

        if (expression == null || (expressionEnd =
            expression.getTextRange().getEndOffset()) != endOffset && (expressionEnd != endOffset - 1 ||
            (sibling = expression.getNextSibling()) == null ||
            sibling.getNode().getElementType() != JSTokenTypes.SEMICOLON)) {
            return null;
        }

        if (expression instanceof JSReferenceExpression refExpr && refExpr.getParent() instanceof JSCallExpression) {
            return null;
        }
        /*if (file.getLanguage() == JavaScriptSupportLoader.JSON) {
            expression = null; // there is no vars in json
        }*/
        return expression;
    }

    protected static JSExpression unparenthesize(JSExpression expression) {
        while (expression instanceof JSParenthesizedExpression parenthesized) {
            expression = parenthesized.getInnerExpression();
        }

        return expression;
    }

    @RequiredReadAction
    public static JSExpression[] findExpressionOccurrences(JSElement scope, JSExpression expr) {
        List<JSExpression> array = new ArrayList<>();
        addExpressionOccurrences(unparenthesize(expr), array, scope);
        return array.toArray(new JSExpression[array.size()]);
    }

    @RequiredReadAction
    protected static void addExpressionOccurrences(JSExpression expr, List<JSExpression> array, PsiElement scope) {
        PsiElement[] children = scope.getChildren();

        for (PsiElement child : children) {
            if (child instanceof JSExpression childExpr) {
                JSExpression childUnparenExpr = unparenthesize(childExpr);

                if (childUnparenExpr != null
                    && PsiEquivalenceUtil.areElementsEquivalent(childUnparenExpr, expr)
                    && !JSResolveUtil.isSelfReference(scope, childExpr)) {
                    array.add(childExpr);
                    continue;
                }
            }

            if (!(child instanceof JSFunction)) {
                addExpressionOccurrences(expr, array, child);
            }
        }
    }

    @Override
    @RequiredWriteAction
    public void invoke(@Nonnull Project project, Editor editor, PsiFile file, DataContext dataContext) {
        if (!editor.getSelectionModel().hasSelection()) {
            editor.getSelectionModel().selectLineAtCaret();
        }
        int start = editor.getSelectionModel().getSelectionStart();
        int end = editor.getSelectionModel().getSelectionEnd();

        JSExpression expression = findIntroducedExpression(file, start, end, editor);
        if (expression == null) {
            return;
        }

        if (!CommonRefactoringUtil.checkReadOnlyStatus(project, file)) {
            return;
        }

        editor.getSelectionModel().removeSelection();
        JSElement scope = findIntroducedScope(expression);
        LOG.assertTrue(scope != null);
        JSExpression[] occurrences = findExpressionOccurrences(scope, expression);
        S settings = getSettings(project, editor, expression, occurrences);
        if (settings == null) {
            return;
        }

        CommandProcessor.getInstance().newCommand()
            .project(project)
            .name(LocalizeValue.ofNullable(getRefactoringName()))
            .inWriteAction()
            .run(() -> doRefactoring(project, editor, new BaseIntroduceContext<>(expression, occurrences, settings)));
    }

    protected static final class BaseIntroduceContext<S> {
        public final S settings;
        final JSExpression[] occurences;
        public final JSExpression expression;

        public BaseIntroduceContext(JSExpression _mainoccurence, JSExpression[] _occurences, S _settings) {
            occurences = _occurences;
            expression = _mainoccurence;
            settings = _settings;
        }
    }

    protected JSElement findIntroducedScope(JSExpression expression) {
        return PsiTreeUtil.getParentOfType(expression, JSFunction.class, JSFile.class, JSEmbeddedContentImpl.class);
    }

    protected abstract String getRefactoringName();

    protected abstract LocalizeValue getCannotIntroduceMessage();

    @Nullable
    @RequiredUIAccess
    protected JSExpression findIntroducedExpression(PsiFile file, int start, int end, Editor editor) {
        JSExpression expression = findExpressionInRange(file, start, end);
        if (expression == null) {
            CommonRefactoringUtil.showErrorHint(
                file.getProject(),
                editor,
                getCannotIntroduceMessage().get(),
                getRefactoringName(),
                null
            );
        }
        return expression;
    }

    @Nullable
    @RequiredUIAccess
    protected S getSettings(Project project, Editor editor, JSExpression expression, JSExpression[] occurrences) {
        ArrayList<RangeHighlighter> highlighters = null;
        if (occurrences.length > 1) {
            highlighters = highlightOccurences(project, editor, occurrences);
        }

        D dialog = createDialog(project, expression, occurrences);
        dialog.show();
        if (highlighters != null) {
            for (RangeHighlighter highlighter : highlighters) {
                HighlightManager.getInstance(project).removeSegmentHighlighter(editor, highlighter);
            }
        }

        if (dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE) {
            return null;
        }

        return createSettings(dialog);
    }

    @SuppressWarnings("unchecked")
    protected S createSettings(D dialog) {
        return (S)dialog;
    }

    protected abstract D createDialog(Project project, JSExpression expression, JSExpression[] occurrences);

    @RequiredWriteAction
    @SuppressWarnings("unchecked")
    private void doRefactoring(Project project, Editor editor, BaseIntroduceContext<S> introduceContext) {
        S settings = introduceContext.settings;
        JSExpression expression = introduceContext.expression;
        JSExpression[] occurrences = introduceContext.occurences;

        boolean replaceAllOccurences = settings.isReplaceAllOccurences();
        String varDeclText = getDeclText(settings);
        PsiFile containingFile = expression.getContainingFile();
        boolean ecma = false;
        if (ecma) {
            String type = settings.getVariableType();
            if (type == null) {
                type = JSResolveUtil.getExpressionType(expression, containingFile);
            }
            varDeclText += ":" + type;
        }

        try {
            T anchorStatement = findAnchor(introduceContext, replaceAllOccurences);
            JSVarStatement declaration = prepareDeclaration(varDeclText, introduceContext, project);

            LOG.assertTrue(anchorStatement != null);

            boolean replacedOriginal = false;

            if (anchorStatement == expression.getParent() && anchorStatement instanceof JSExpressionStatement anchorExpression) {
                declaration = (JSVarStatement)anchorExpression.replace(declaration);
                editor.getCaretModel().moveToOffset(declaration.getTextRange().getEndOffset());
                replacedOriginal = true;
            }
            else {
                JSExpression oldExpression = expression;
                TextRange expressionTextRange = expression.getTextRange();
                TextRange statementTextRange = anchorStatement.getTextRange();

                RangeMarker marker = editor.getDocument().createRangeMarker(expressionTextRange);

                // Adding declaration to anchorStatement may invalidate original expression so we need to find it in new tree
                T jsStatement = addStatementBefore(anchorStatement, declaration);

                if (!expression.isValid()) {
                    T newAnchorStatement = (T)PsiTreeUtil.getNextSiblingOfType(jsStatement, anchorStatement.getClass());
                    int relativeOffset = marker.getStartOffset() - statementTextRange.getStartOffset();
                    JSExpression newExpression =
                        PsiTreeUtil.getParentOfType(newAnchorStatement.findElementAt(relativeOffset), oldExpression.getClass());

                    if (newExpression == null) {
                        assert false : "Could not find " + oldExpression.getClass() + " in " + newAnchorStatement.getText() + " with offset " + marker
                            .getStartOffset();
                    }

                    while (newExpression.getTextRange().getLength() != expressionTextRange.getLength()) {
                        JSExpression candidateExpression = PsiTreeUtil.getParentOfType(newExpression, oldExpression.getClass());
                        if (candidateExpression == null) {
                            break;
                        }
                        if (candidateExpression.getTextRange().getStartOffset() - newAnchorStatement.getTextRange()
                            .getStartOffset() != marker.getStartOffset()) {
                            break;
                        }
                        newExpression = candidateExpression;
                    }

                    for (int i = 0; i < occurrences.length; ++i) {
                        if (occurrences[i] == oldExpression) {
                            occurrences[i] = newExpression;
                            break;
                        }
                    }

                    expression = newExpression;
                }
            }

            JSExpression refExpr = JSChangeUtil.createExpressionFromText(project, settings.getVariableName());
            if (replaceAllOccurences) {
                List<JSExpression> toHighight = new ArrayList<>();
                for (JSExpression occurence : occurrences) {
                    if (occurence != expression || !replacedOriginal) {
                        toHighight.add(occurence.replace(refExpr));
                    }
                    else {
                        toHighight.add(declaration.getVariables()[0].getInitializer());
                    }
                }

                highlightOccurences(project, editor, toHighight.toArray(new JSExpression[toHighight.size()]));
            }
            else if (!replacedOriginal) {
                expression.replace(refExpr);
            }
        }
        catch (IncorrectOperationException e) {
            LOG.error(e);
        }
    }

    @RequiredWriteAction
    protected JSVarStatement prepareDeclaration(String varDeclText, BaseIntroduceContext<S> context, Project project)
        throws IncorrectOperationException {
        JSVarStatement declaration = (JSVarStatement)JSChangeUtil.createStatementFromText(
            project,
            varDeclText + " = 0" + JSChangeUtil.getSemicolon(project)
        ).getPsi();
        declaration.getVariables()[0].getInitializer().replace(context.expression);
        return declaration;
    }

    protected String getDeclText(S settings) {
        return "var " + settings.getVariableName();
    }

    @RequiredWriteAction
    @SuppressWarnings("unchecked")
    protected T addStatementBefore(T anchorStatement, JSVarStatement declaration) throws IncorrectOperationException {
        return (T)((JSStatement)anchorStatement).addStatementBefore(declaration);
    }

    @SuppressWarnings("unchecked")
    protected T findAnchor(BaseIntroduceContext<S> context, boolean replaceAllOccurences) {
        JSStatement anchorStatement = replaceAllOccurences
            ? getAnchorToInsert(context.occurences)
            : PsiTreeUtil.getParentOfType(context.expression, JSStatement.class);
        if (anchorStatement instanceof JSVarStatement varStatement
            && varStatement.getParent() instanceof JSStatement statement
            && !(statement instanceof JSBlockStatement)) {
            anchorStatement = statement;
        }
        return (T)anchorStatement;
    }

    private static ArrayList<RangeHighlighter> highlightOccurences(Project project, Editor editor, JSExpression[] occurences) {
        HighlightManager highlightManager = HighlightManager.getInstance(project);
        ArrayList<RangeHighlighter> result = new ArrayList<>();
        highlightManager.addOccurrenceHighlights(editor, occurences, EditorColors.SEARCH_RESULT_ATTRIBUTES, true, result);
        return result;
    }

    private static JSStatement getAnchorToInsert(JSExpression[] expressions) {
        JSElement place = expressions[0];
        next:
        do {
            JSStatement statement = PsiTreeUtil.getParentOfType(place, JSStatement.class); //this is the first expression textually
            LOG.assertTrue(statement != null);

            PsiElement parent = statement.getParent();
            for (JSExpression expression : expressions) {
                if (!PsiTreeUtil.isAncestor(parent, expression, true)) {
                    place = statement;
                    continue next;
                }
            }

            return statement;
        }
        while (true);
    }

    protected static JSElement findClassAnchor(PsiElement expression) {
        PsiElement nearestParent = PsiTreeUtil.getParentOfType(expression, JSVarStatement.class, JSFunction.class);
        while (nearestParent != null) {
            PsiElement nextParent = PsiTreeUtil.getParentOfType(nearestParent, JSVarStatement.class, JSFunction.class);
            if (nextParent == null) {
                break;
            }
            nearestParent = nextParent;
        }

        if (nearestParent != null) {
            return (JSElement)nearestParent;
        }

        JSElement parent = PsiTreeUtil.getParentOfType(expression, JSFile.class, JSClass.class);

        if (parent instanceof JSFile jsFile && JSResolveUtil.getClassReferenceForXmlFromContext(jsFile) instanceof JSClass jsClass) {
            parent = jsClass;
        }

        return parent;
    }

    @RequiredWriteAction
    protected static JSElement addToClassAnchor(JSElement anchorStatement, JSVarStatement declaration) throws IncorrectOperationException {
        if (!(anchorStatement instanceof JSClass)) {
            JSElement element = findClassAnchor(anchorStatement);
            return (JSElement)element.addBefore(declaration, anchorStatement);
        }
        return (JSElement)anchorStatement.add(declaration);
    }

    @Override
    @RequiredUIAccess
    public void invoke(@Nonnull Project project, @Nonnull PsiElement[] elements, DataContext dataContext) {
        throw new RuntimeException("Not implemented");
    }
}