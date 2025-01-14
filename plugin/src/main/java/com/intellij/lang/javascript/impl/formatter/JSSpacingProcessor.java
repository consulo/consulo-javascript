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

package com.intellij.lang.javascript.impl.formatter;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.impl.JSNodeVisitor;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSImportStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.codeStyle.CodeStyleSettings;
import consulo.language.codeStyle.CommonCodeStyleSettings;
import consulo.language.codeStyle.Spacing;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.xml.psi.xml.XmlText;

/**
 * @author ven
 */
public class JSSpacingProcessor extends JSNodeVisitor {
    private ASTNode myParent;
    private ASTNode myChild1;
    private ASTNode myChild2;
    private final CommonCodeStyleSettings mySettings;
    private Spacing myResult;
    private final IElementType type1;
    private final IElementType type2;

    public JSSpacingProcessor(ASTNode parent, ASTNode child1, ASTNode child2, CommonCodeStyleSettings settings) {
        myParent = parent;
        myChild1 = child1;
        myChild2 = child2;
        mySettings = settings;
        type1 = child1.getElementType();
        type2 = child2.getElementType();
        visit(parent);
    }

    public Spacing getResult() {
        return myResult;
    }

    @Override
    public void visitObjectLiteralExpression(ASTNode node) {
        if (((type1 == JSTokenTypes.LBRACE && type2 != JSTokenTypes.RBRACE)
            || type1 == JSTokenTypes.COMMA
            || (type1 != JSTokenTypes.LBRACE && type2 == JSTokenTypes.RBRACE))
            && shouldFormatObjectLiteralExpression()) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
    }

    private boolean shouldFormatObjectLiteralExpression() {
        IElementType grandParentType = myParent.getTreeParent().getElementType();
        return grandParentType == JSElementTypes.ARRAY_LITERAL_EXPRESSION;
    }

    @Override
    @RequiredReadAction
    public void visitArrayLiteralExpression(ASTNode node) {
        if (((type1 == JSTokenTypes.LBRACKET && type2 != JSTokenTypes.RBRACKET)
            || type1 == JSTokenTypes.COMMA
            || (type1 != JSTokenTypes.LBRACKET && type2 == JSTokenTypes.RBRACKET))
            && shouldFormatArrayLiteralExpression()) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
    }

    @RequiredReadAction
    private boolean shouldFormatArrayLiteralExpression() {
        JSExpression jsExpression = PsiTreeUtil.findChildOfType(myParent.getPsi(), JSExpression.class);
        if (jsExpression != null) {
            IElementType type = PsiUtilCore.getElementType(jsExpression);
            return type == JSElementTypes.ARRAY_LITERAL_EXPRESSION || type == JSElementTypes.OBJECT_LITERAL_EXPRESSION;
        }
        return false;
    }

    @Override
    public void visitXmlLiteralExpression(ASTNode node) {
        if ((type2 == JSElementTypes.XML_LITERAL_EXPRESSION
            || type1 == JSTokenTypes.XML_START_TAG_LIST && type2 != JSTokenTypes.XML_END_TAG_LIST
            || type1 == JSElementTypes.XML_LITERAL_EXPRESSION)
            && shouldFormatXmlLiteralExpression()) {
            myResult = Spacing.createSpacing(0, 0, 1, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
        }
    }

    private boolean shouldFormatXmlLiteralExpression() {
        return true;
    }

    @Override
    public void visitAttributeList(final ASTNode node) {
        if (type1 == JSElementTypes.ATTRIBUTE || type2 == JSElementTypes.ATTRIBUTE) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
        else {
            myResult = Spacing.createSpacing(
                1,
                1,
                0,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
    }

    @Override
    public void visitEmbeddedContent(final ASTNode node) {
        if (type2 == JSTokenTypes.END_OF_LINE_COMMENT) {
            myResult = Spacing.createSpacing(
                0,
                Integer.MAX_VALUE,
                0,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
        else if (JSElementTypes.SOURCE_ELEMENTS.contains(type1)
            || JSElementTypes.SOURCE_ELEMENTS.contains(type2) && type1 != JSTokenTypes.DOT
            || type2 == JSTokenTypes.RBRACE) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
    }

    @Override
    public void visitParameterList(ASTNode node) {
        if (type1 == JSTokenTypes.LPAR && type2 == JSTokenTypes.RPAR) {
            setSingleSpace(false);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_METHOD_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.COMMA) {
            setSingleSpace(mySettings.SPACE_AFTER_COMMA);
        }
        else if (type2 == JSTokenTypes.COMMA) {
            setSingleSpace(mySettings.SPACE_BEFORE_COMMA);
        }
    }

    @Override
    @RequiredReadAction
    public void visitPackageStatement(ASTNode node) {
        if (shouldMakeLBraceOnNextLine()) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
            return;
        }
        processBlock(node);
    }

    @Override
    @RequiredReadAction
    public void visitClass(ASTNode node) {
        if (shouldMakeLBraceOnNextLine()) {
            myResult = Spacing.createSpacing(
                0,
                0,
                1,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
            return;
        }
        processBlock(node);
    }

    private boolean shouldMakeLBraceOnNextLine() {
        return type2 == JSTokenTypes.LBRACE && mySettings.CLASS_BRACE_STYLE == CodeStyleSettings.NEXT_LINE;
    }

    @Override
    @RequiredReadAction
    public void visitBlock(ASTNode node) {
        processBlock(node);
    }

    @RequiredReadAction
    private void processBlock(ASTNode node) {
        if (JSElementTypes.SOURCE_ELEMENTS.contains(type1) || JSElementTypes.SOURCE_ELEMENTS.contains(type2) ||
            type2 == JSTokenTypes.RBRACE) {
            if (isInjectedJSHack(type1, type2) || isInjectedJSHack(type2, type1)
                || (type2 == JSTokenTypes.END_OF_LINE_COMMENT
                && isInlineEndOfLineCommentOnLeft() && type1 != JSElementTypes.ES4_IMPORT_STATEMENT)) {
                myResult = Spacing.getReadOnlySpacing();
            }
            else {
                boolean keepOneLine = myParent.getPsi() instanceof JSFunction
                    ? mySettings.KEEP_SIMPLE_METHODS_IN_ONE_LINE
                    : mySettings.KEEP_SIMPLE_BLOCKS_IN_ONE_LINE;

                if (keepOneLine && (type1 == JSTokenTypes.LBRACE || type2 == JSTokenTypes.RBRACE)) {
                    myResult = Spacing.createDependentLFSpacing(0,
                        1,
                        node.getTextRange(),
                        mySettings.KEEP_LINE_BREAKS,
                        mySettings.KEEP_BLANK_LINES_IN_CODE
                    );
                }
                else if ((JSElementTypes.ES4_IMPORT_STATEMENT == type1 && JSElementTypes.ES4_IMPORT_STATEMENT != type2
                    && JSTokenTypes.RBRACE != type2)
                    || (JSElementTypes.ES4_IMPORT_STATEMENT != type1 && JSTokenTypes.LBRACE != type1
                    && JSElementTypes.ES4_IMPORT_STATEMENT == type2)) {
                    myResult = Spacing.createSpacing(
                        0,
                        0,
                        2,
                        mySettings.KEEP_LINE_BREAKS,
                        mySettings.KEEP_BLANK_LINES_IN_CODE
                    );
                }
                else if (JSElementTypes.ES4_IMPORT_STATEMENT == type1 && JSElementTypes.ES4_IMPORT_STATEMENT == type2) {
                    myResult = getSpacingBetweenImports();
                }
                else if (shouldPlaceExtraLinesAroundMethod()) {
                    myResult = Spacing.createSpacing(
                        0,
                        0,
                        1 + mySettings.BLANK_LINES_AROUND_METHOD,
                        mySettings.KEEP_LINE_BREAKS,
                        mySettings.KEEP_BLANK_LINES_IN_CODE
                    );
                }
                else {
                    myResult = Spacing.createSpacing(
                        0,
                        0,
                        1,
                        mySettings.KEEP_LINE_BREAKS,
                        mySettings.KEEP_BLANK_LINES_IN_CODE
                    );
                }
            }
        }
    }

    private boolean shouldPlaceExtraLinesAroundMethod() {
        return (type1 == JSElementTypes.FUNCTION_DECLARATION && type2 != JSTokenTypes.RBRACE)
            || (type1 == JSElementTypes.VAR_STATEMENT && type2 == JSElementTypes.FUNCTION_DECLARATION);
    }

    @RequiredReadAction
    private Spacing getSpacingBetweenImports() {
        String fqn1 = ((JSImportStatement)myChild1.getPsi()).getImportText();
        String fqn2 = ((JSImportStatement)myChild2.getPsi()).getImportText();
        String rootPackage1 = fqn1 != null && fqn1.contains(".") ? fqn1.substring(0, fqn1.indexOf(".")) : null;
        String rootPackage2 = fqn2 != null && fqn2.contains(".") ? fqn2.substring(0, fqn2.indexOf(".")) : null;
        boolean lineBreak = rootPackage1 != null && rootPackage2 != null && !rootPackage1.equals(rootPackage2);
        return Spacing.createSpacing(
            0,
            0,
            lineBreak ? 2 : 1,
            mySettings.KEEP_LINE_BREAKS,
            mySettings.KEEP_BLANK_LINES_IN_CODE
        );
    }

    private static boolean isInjectedJSHack(IElementType type1, IElementType type2) {
        // Following code attempts NOT to reformat '@xxx:bbb.ccc' in var initializers
        return type1 == JSTokenTypes.BAD_CHARACTER && JSElementTypes.SOURCE_ELEMENTS.contains(type2);
    }

    @Override
    @RequiredReadAction
    public void visitFile(ASTNode node) {
        if (JSElementTypes.SOURCE_ELEMENTS.contains(type1) || JSElementTypes.SOURCE_ELEMENTS.contains(type2)) {
            if (type2 == JSTokenTypes.END_OF_LINE_COMMENT
                && isInlineEndOfLineCommentOnLeft()
                && type1 != JSElementTypes.ES4_IMPORT_STATEMENT) {
                myResult = Spacing.getReadOnlySpacing();
            }
            else if ((JSElementTypes.ES4_IMPORT_STATEMENT == type1 && JSElementTypes.ES4_IMPORT_STATEMENT != type2)
                || (JSElementTypes.ES4_IMPORT_STATEMENT != type1 && JSElementTypes.ES4_IMPORT_STATEMENT == type2)) {
                myResult = Spacing.createSpacing(
                    0,
                    0,
                    2,
                    mySettings.KEEP_LINE_BREAKS,
                    mySettings.KEEP_BLANK_LINES_IN_CODE
                );
            }
            else if (JSElementTypes.ES4_IMPORT_STATEMENT == type1 && JSElementTypes.ES4_IMPORT_STATEMENT == type2) {
                myResult = getSpacingBetweenImports();
            }
            else if (shouldPlaceExtraLinesAroundMethod() && node.getPsi().getContext() instanceof XmlText) {
                myResult = Spacing.createSpacing(
                    0,
                    0,
                    1 + mySettings.BLANK_LINES_AROUND_METHOD,
                    mySettings.KEEP_LINE_BREAKS,
                    mySettings.KEEP_BLANK_LINES_IN_CODE
                );
            }
            else {
                myResult = Spacing.createSpacing(
                    0,
                    0,
                    1,
                    mySettings.KEEP_LINE_BREAKS,
                    mySettings.KEEP_BLANK_LINES_IN_CODE
                );
            }
        }
    }

    private boolean isInlineEndOfLineCommentOnLeft() {
        ASTNode prev = myChild2.getTreePrev();
        return prev == myChild1
            || prev != null && prev.getPsi() instanceof PsiWhiteSpace && !prev.textContains('\n');
    }

    @Override
    public void visitFunctionDeclaration(ASTNode node) {
        if (type1 == JSTokenTypes.FUNCTION_KEYWORD && type2 == JSElementTypes.REFERENCE_EXPRESSION) {
            setSingleSpace(true);
        }
        else if (type1 == JSElementTypes.REFERENCE_EXPRESSION && type2 == JSElementTypes.PARAMETER_LIST) {
            setSingleSpace(mySettings.SPACE_BEFORE_METHOD_PARENTHESES);
        }
        else if (type2 == JSElementTypes.BLOCK_STATEMENT) {
            setBraceSpace(mySettings.SPACE_BEFORE_METHOD_LBRACE, mySettings.METHOD_BRACE_STYLE, myChild1.getTextRange());
        }
        else if (type1 == JSElementTypes.ATTRIBUTE_LIST && type2 == JSTokenTypes.FUNCTION_KEYWORD) {
            setSingleSpace(true);
        }
    }

    @Override
    public void visitFunctionExpression(ASTNode node) {
        visitFunctionDeclaration(node);
    }

    @Override
    public void visitReferenceExpression(ASTNode node) {
        if (type1 == JSTokenTypes.NEW_KEYWORD) {
            setSingleSpace(true);
        }
        else {
            setSingleSpace(false); // a.b should not have spaces before and after dot
        }
    }

    @Override
    public void visitDocComment(ASTNode node) {
        //myResult = Spacing.createKeepingFirstColumnSpacing(
        //    0,
        //    Integer.MAX_VALUE,
        //    mySettings.KEEP_LINE_BREAKS,
        //    mySettings.KEEP_BLANK_LINES_IN_CODE
        //);
    }

    @Override
    public void visitIfStatement(ASTNode node) {
        if (type1 == JSTokenTypes.IF_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_IF_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_IF_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR && type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_IF_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type2 == JSTokenTypes.ELSE_KEYWORD) {
            setLineBreakSpace(mySettings.ELSE_ON_NEW_LINE);
        }
        else if (type1 == JSTokenTypes.ELSE_KEYWORD && type2 == JSElementTypes.BLOCK_STATEMENT) {
            setBraceSpace(mySettings.SPACE_BEFORE_ELSE_LBRACE, mySettings.BRACE_STYLE, null);
        }
    }

    @Override
    public void visitCallExpression(ASTNode node) {
        if (type2 == JSElementTypes.ARGUMENT_LIST) {
            setSingleSpace(mySettings.SPACE_BEFORE_METHOD_CALL_PARENTHESES);
        }
    }

    @Override
    public void visitNewExpression(ASTNode node) {
        if (type1 == JSTokenTypes.NEW_KEYWORD) {
            setSingleSpace(true);
        }
        else if (type2 == JSElementTypes.ARGUMENT_LIST) {
            setSingleSpace(mySettings.SPACE_BEFORE_METHOD_CALL_PARENTHESES);
        }
    }

    @Override
    public void visitForStatement(ASTNode node) {
        if (type1 == JSTokenTypes.SEMICOLON) {
            setSingleSpace(true);
        }
        else if (type2 == JSTokenTypes.SEMICOLON) {
            setSingleSpace(mySettings.SPACE_BEFORE_SEMICOLON);
        }

        if (type1 == JSTokenTypes.FOR_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_FOR_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR && type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_FOR_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_FOR_PARENTHESES);
        }
    }

    @Override
    public void visitDoWhileStatement(ASTNode node) {
        if (type2 == JSTokenTypes.WHILE_KEYWORD) {
            if (mySettings.WHILE_ON_NEW_LINE) {
                myResult = Spacing.createSpacing(
                    0,
                    0,
                    1,
                    mySettings.KEEP_LINE_BREAKS,
                    mySettings.KEEP_BLANK_LINES_IN_CODE
                );
            }
            else {
                myResult = Spacing.createSpacing(
                    1,
                    1,
                    0,
                    mySettings.KEEP_LINE_BREAKS,
                    mySettings.KEEP_BLANK_LINES_IN_CODE
                );
            }
        }
        else if (type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_WHILE_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_WHILE_PARENTHESES);
        }

        if (type1 == JSTokenTypes.DO_KEYWORD && type2 == JSElementTypes.BLOCK_STATEMENT) {
            setBraceSpace(true, mySettings.BRACE_STYLE, null);
        }
    }

    @Override
    public void visitForInStatement(ASTNode node) {
        if (type1 == JSTokenTypes.VAR_KEYWORD || type2 == JSTokenTypes.VAR_KEYWORD) {
            setSingleSpace(true);
        }
        else if (type1 == JSTokenTypes.FOR_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_FOR_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR && type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_FOR_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_FOR_PARENTHESES);
        }
    }

    @Override
    public void visitWhileStatement(ASTNode node) {
        if (type1 == JSTokenTypes.WHILE_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_WHILE_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR && type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_WHILE_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_WHILE_PARENTHESES);
        }
    }

    @Override
    public void visitWithStatement(ASTNode node) {
        if (type1 == JSTokenTypes.WITH_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_WHILE_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR && type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_WHILE_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_WHILE_PARENTHESES);
        }
    }

    public static TokenSet NOT_A_PACKAGE_CONTENT = TokenSet.create(JSTokenTypes.PACKAGE_KEYWORD, JSElementTypes.REFERENCE_EXPRESSION,
        JSTokenTypes.LBRACE, JSTokenTypes.RBRACE
    );

    @Override
    public void visitTryStatement(ASTNode node) {
        if (type1 == JSTokenTypes.TRY_KEYWORD && type2 == JSElementTypes.BLOCK_STATEMENT) {
            setBraceSpace(mySettings.SPACE_BEFORE_TRY_LBRACE, mySettings.BRACE_STYLE, null);
        }
        else if (type2 == JSElementTypes.CATCH_BLOCK) {
            setLineBreakSpace(mySettings.CATCH_ON_NEW_LINE);
        }
        else if (type2 == JSTokenTypes.FINALLY_KEYWORD) {
            setLineBreakSpace(mySettings.FINALLY_ON_NEW_LINE);
        }
        else if (type1 == JSTokenTypes.FINALLY_KEYWORD) {
            setBraceSpace(mySettings.SPACE_BEFORE_FINALLY_LBRACE, mySettings.BRACE_STYLE, null);
        }
    }

    @Override
    public void visitCatchBlock(ASTNode node) {
        if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_CATCH_PARENTHESES);
        }

        if (type1 == JSTokenTypes.CATCH_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_CATCH_PARENTHESES);
        }

        if (type2 == JSElementTypes.BLOCK_STATEMENT) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild2.getTextRange().getStartOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_CATCH_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
    }

    @Override
    public void visitSwitchStatement(ASTNode node) {
        if (type1 == JSTokenTypes.SWITCH_KEYWORD && type2 == JSTokenTypes.LPAR) {
            setSingleSpace(mySettings.SPACE_BEFORE_SWITCH_PARENTHESES);
        }
        else if (type1 == JSTokenTypes.RPAR) {
            TextRange dependentRange = new TextRange(myParent.getStartOffset(), myChild1.getTextRange().getEndOffset());
            setBraceSpace(mySettings.SPACE_BEFORE_SWITCH_LBRACE, mySettings.BRACE_STYLE, dependentRange);
        }
        else if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(mySettings.SPACE_WITHIN_SWITCH_PARENTHESES);
        }
    }

    @Override
    public void visitArgumentList(ASTNode node) {
        if (type1 == JSTokenTypes.LPAR || type2 == JSTokenTypes.RPAR) {
            setSingleSpace(false);
        }
        else if (type1 == JSTokenTypes.COMMA) {
            setSingleSpace(mySettings.SPACE_AFTER_COMMA);
        }
        else if (type2 == JSTokenTypes.COMMA) {
            setSingleSpace(mySettings.SPACE_BEFORE_COMMA);
        }
    }

    @Override
    public void visitStatement(ASTNode node) {
        if (type2 == JSTokenTypes.SEMICOLON) {
            setSingleSpace(false);
        }
    }

    @Override
    public void visitVarStatement(ASTNode node) {
        if (type1 == JSTokenTypes.VAR_KEYWORD) {
            setSingleSpace(true);
        }
    }

    @Override
    public void visitVariable(ASTNode node) {
        if (type1 == JSTokenTypes.EQ || type2 == JSTokenTypes.EQ) { // Initializer
            setSingleSpace(mySettings.SPACE_AROUND_ASSIGNMENT_OPERATORS);
        }
    }

    @Override
    public void visitBinaryExpression(ASTNode node) {
        IElementType opSign = null;
        if (JSTokenTypes.OPERATIONS.contains(type1)) {
            opSign = type1;
        }
        else if (JSTokenTypes.OPERATIONS.contains(type2)) {
            opSign = type2;
        }

        if (opSign != null) {
            setSingleSpace(getSpaceAroundOption(opSign));
        }
    }

    @Override
    public void visitConditionalExpression(ASTNode node) {
        if (type1 == JSTokenTypes.QUEST) {
            setSingleSpace(mySettings.SPACE_AFTER_QUEST);
        }
        else if (type2 == JSTokenTypes.QUEST) {
            setSingleSpace(mySettings.SPACE_BEFORE_QUEST);
        }
        else if (type1 == JSTokenTypes.COLON) {
            setSingleSpace(mySettings.SPACE_AFTER_COLON);
        }
        else if (type2 == JSTokenTypes.COLON) {
            setSingleSpace(mySettings.SPACE_BEFORE_COLON);
        }
    }

    private boolean getSpaceAroundOption(IElementType opSign) {
        boolean option = false;
        if (JSTokenTypes.ADDITIVE_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_ADDITIVE_OPERATORS;
        }
        else if (JSTokenTypes.MULTIPLICATIVE_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_MULTIPLICATIVE_OPERATORS;
        }
        else if (JSTokenTypes.ASSIGNMENT_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_ASSIGNMENT_OPERATORS;
        }
        else if (JSTokenTypes.EQUALITY_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_EQUALITY_OPERATORS;
        }
        else if (JSTokenTypes.RELATIONAL_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_RELATIONAL_OPERATORS;
        }
        else if (JSTokenTypes.SHIFT_OPERATIONS.contains(opSign)) {
            option = mySettings.SPACE_AROUND_BITWISE_OPERATORS;
        }
        else if (opSign == JSTokenTypes.ANDAND || opSign == JSTokenTypes.OROR) {
            option = mySettings.SPACE_AROUND_LOGICAL_OPERATORS;
        }
        else if (opSign == JSTokenTypes.OR || opSign == JSTokenTypes.AND || opSign == JSTokenTypes.XOR) {
            option = mySettings.SPACE_AROUND_BITWISE_OPERATORS;
        }
        else if (opSign == JSTokenTypes.IS_KEYWORD || opSign == JSTokenTypes.AS_KEYWORD) {
            option = true;
        }
        return option;
    }

    private void setSingleSpace(boolean needSpace) {
        int spaces = needSpace ? 1 : 0;
        myResult = Spacing.createSpacing(spaces, spaces, 0, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
    }

    private void setBraceSpace(boolean needSpaceSetting, int braceStyleSetting, TextRange textRange) {
        int spaces = needSpaceSetting ? 1 : 0;
        if (braceStyleSetting == CodeStyleSettings.NEXT_LINE_IF_WRAPPED && textRange != null) {
            myResult = Spacing.createDependentLFSpacing(
                spaces,
                spaces,
                textRange,
                mySettings.KEEP_LINE_BREAKS,
                mySettings.KEEP_BLANK_LINES_IN_CODE
            );
        }
        else {
            int lineBreaks = braceStyleSetting == CodeStyleSettings.END_OF_LINE ? 0 : 1;
            myResult = Spacing.createSpacing(spaces, spaces, lineBreaks, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
        }
    }

    private void setLineBreakSpace(boolean needLineBreak) {
        int breaks = needLineBreak || myChild1.getElementType() == JSTokenTypes.END_OF_LINE_COMMENT ? 1 : 0;
        myResult = Spacing.createSpacing(1, 1, breaks, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
    }
}
