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

package consulo.javascript.run.debug.v8;

import consulo.execution.debug.XDebuggerUtil;
import consulo.execution.debug.XSourcePosition;
import consulo.execution.debug.evaluation.XDebuggerEvaluator;
import consulo.execution.debug.frame.XCompositeNode;
import consulo.execution.debug.frame.XStackFrame;
import consulo.execution.debug.frame.XValueChildrenList;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.ui.ex.ColoredTextContainer;
import consulo.ui.ex.SimpleTextAttributes;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.chromium.sdk.*;

import java.util.Collections;
import java.util.List;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8StackFrame extends XStackFrame {
    private final CallFrame myCallFrame;

    public V8StackFrame(CallFrame callFrame) {
        myCallFrame = callFrame;
    }

    @Nullable
    @Override
    public XDebuggerEvaluator getEvaluator() {
        return new XDebuggerEvaluator() {
            @Override
            public void evaluate(@Nonnull final String expression, @Nonnull final XEvaluationCallback callback, @Nullable XSourcePosition expressionPosition) {
                final JsEvaluateContext evaluateContext = myCallFrame.getEvaluateContext();
                evaluateContext.evaluateSync(expression, Collections.<String, JsValue>emptyMap(), new JsEvaluateContext.EvaluateCallback() {
                    @Override
                    public void success(JsEvaluateContext.ResultOrException e) {
                        JsValue result = e.getResult() == null ? e.getException() : e.getResult();
                        if (result != null) {
                            callback.evaluated(new V8WatchValue(evaluateContext, expression, result));
                        }
                        else {
                            callback.errorOccurred("bad expression");
                        }
                    }

                    @Override
                    public void failure(Exception e) {
                        callback.errorOccurred(e.getMessage());
                    }
                });
            }

            @Override
            public boolean isCodeFragmentEvaluationSupported() {
                return false;
            }
        };
    }

    @Override
    public void computeChildren(@Nonnull XCompositeNode node) {
        JsEvaluateContext evaluateContext = myCallFrame.getEvaluateContext();
        List<? extends JsScope> variableScopes = myCallFrame.getVariableScopes();

        XValueChildrenList valueChildrenList = new XValueChildrenList();

        for (JsScope variableScope : variableScopes) {
            switch (variableScope.getType()) {
                case LOCAL:
                    JsScope.Declarative declarative = variableScope.asDeclarativeScope();
                    if (declarative == null) {
                        break;
                    }
                    for (JsVariable jsVariable : declarative.getVariables()) {
                        V8VariableValue.addValue(valueChildrenList, evaluateContext, jsVariable);
                    }
                    break;
            }
        }
        node.addChildren(valueChildrenList, true);
    }

    @Override
    public void customizePresentation(ColoredTextContainer component) {
        TextStreamPosition statementStartPosition = myCallFrame.getStatementStartPosition();

        XSourcePosition position = getSourcePosition();
        if (position != null) {
            component.append(position.getFile().getName(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
        }
        else {
            component.append(myCallFrame.getScript().getName(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
        }

        component.append(":" + (statementStartPosition.getLine() + 1), SimpleTextAttributes.REGULAR_ATTRIBUTES);
    }

    @Override
    @Nullable
    public XSourcePosition getSourcePosition() {
        TextStreamPosition statementStartPosition = myCallFrame.getStatementStartPosition();
        if (myCallFrame.getScript().getName() == null) {
            return null;
        }
        return XDebuggerUtil.getInstance().createPosition(V8ScriptUtil.toVirtualFile(myCallFrame.getScript(), true), statementStartPosition.getLine());
    }
}
