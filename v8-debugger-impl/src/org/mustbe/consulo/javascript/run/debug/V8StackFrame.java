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

package org.mustbe.consulo.javascript.run.debug;

import java.util.Collections;
import java.util.List;

import org.chromium.sdk.CallFrame;
import org.chromium.sdk.JsEvaluateContext;
import org.chromium.sdk.JsScope;
import org.chromium.sdk.JsValue;
import org.chromium.sdk.JsVariable;
import org.chromium.sdk.TextStreamPosition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.icons.AllIcons;
import com.intellij.ui.ColoredTextContainer;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.XDebuggerUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XStackFrame;
import com.intellij.xdebugger.frame.XValueChildrenList;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8StackFrame extends XStackFrame
{
	private final CallFrame myCallFrame;

	public V8StackFrame(CallFrame callFrame)
	{
		myCallFrame = callFrame;
	}

	@Nullable
	@Override
	public XDebuggerEvaluator getEvaluator()
	{
		return new XDebuggerEvaluator()
		{
			@Override
			public void evaluate(@NotNull final String expression, @NotNull final XEvaluationCallback callback, @Nullable XSourcePosition expressionPosition)
			{
				final JsEvaluateContext evaluateContext = myCallFrame.getEvaluateContext();
				evaluateContext.evaluateSync(expression, Collections.<String, JsValue>emptyMap(), new JsEvaluateContext.EvaluateCallback()
				{
					@Override
					public void success(JsEvaluateContext.ResultOrException e)
					{
						JsValue result = e.getResult() == null ? e.getException() : e.getResult();
						if(result != null)
						{
							callback.evaluated(new V8WatchValue(evaluateContext, expression, result));
						}
						else
						{
							callback.errorOccurred("bad expression");
						}
					}

					@Override
					public void failure(Exception e)
					{
						callback.errorOccurred(e.getMessage());
					}
				});
			}

			@Override
			public boolean isCodeFragmentEvaluationSupported()
			{
				return false;
			}
		};
	}

	@Override
	public void computeChildren(@NotNull XCompositeNode node)
	{
		JsEvaluateContext evaluateContext = myCallFrame.getEvaluateContext();
		List<? extends JsScope> variableScopes = myCallFrame.getVariableScopes();

		XValueChildrenList valueChildrenList = new XValueChildrenList();

		for(JsScope variableScope : variableScopes)
		{
			switch(variableScope.getType())
			{
				case LOCAL:
					JsScope.Declarative declarative = variableScope.asDeclarativeScope();
					if(declarative == null)
					{
						break;
					}
					for(JsVariable jsVariable : declarative.getVariables())
					{
						V8VariableValue.addValue(valueChildrenList, evaluateContext, jsVariable);
					}
					break;
			}
		}
		node.addChildren(valueChildrenList, true);
	}

	@Override
	public void customizePresentation(ColoredTextContainer component)
	{
		TextStreamPosition statementStartPosition = myCallFrame.getStatementStartPosition();

		XSourcePosition position = getSourcePosition();
		if(position != null)
		{
			component.append(position.getFile().getName(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
		}
		else
		{
			component.append(myCallFrame.getScript().getName(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
		}

		component.append(":" + (statementStartPosition.getLine() + 1), SimpleTextAttributes.REGULAR_ATTRIBUTES);
		component.setIcon(AllIcons.Debugger.StackFrame);
	}

	@Override
	@Nullable
	public XSourcePosition getSourcePosition()
	{
		TextStreamPosition statementStartPosition = myCallFrame.getStatementStartPosition();
		if(myCallFrame.getScript().getName() == null)
		{
			return null;
		}
		return XDebuggerUtil.getInstance().createPosition(V8ScriptUtil.toVirtualFile(myCallFrame.getScript(), true), statementStartPosition.getLine());
	}
}
