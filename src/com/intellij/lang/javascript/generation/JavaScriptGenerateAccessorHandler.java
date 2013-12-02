package com.intellij.lang.javascript.generation;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JComponent;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.validation.JSAnnotatingVisitor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveState;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.ui.NonFocusableCheckBox;
import com.intellij.util.IncorrectOperationException;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 19, 2008
 *         Time: 1:11:10 AM
 */
class JavaScriptGenerateAccessorHandler extends BaseJSGenerateHandler
{
	private GenerationMode mode;
	private JCheckBox myCreateBindableProperties;

	static enum GenerationMode
	{
		GETTERS, SETTERS, GETTERS_AND_SETTERS, CONSTRUCTOR, TOSTRING
	}

	JavaScriptGenerateAccessorHandler(GenerationMode _mode)
	{
		mode = _mode;
	}

	protected String getTitleKey()
	{
		return mode == GenerationMode.GETTERS ? "generate.getter.fields.chooser.title" : mode == GenerationMode.SETTERS ? "generate.setter.fields.chooser" +
				".title" : mode == GenerationMode.GETTERS_AND_SETTERS ? "generate.getter.setter.chooser.title" : mode == GenerationMode.TOSTRING ? "generate.to" +
				".string.chooser.title" : "generate.constructor.fields.chooser.title";
	}

	@Override
	protected void appendOwnOptions(List<JComponent> jComponentList)
	{
		super.appendOwnOptions(jComponentList);
		if(mode == GenerationMode.GETTERS || mode == GenerationMode.GETTERS_AND_SETTERS || mode == GenerationMode.SETTERS)
		{
			if(!ApplicationManager.getApplication().isUnitTestMode())
			{
				myCreateBindableProperties = new NonFocusableCheckBox(JSBundle.message("generate.getter.fields.bindable.properties"));
				jComponentList.add(myCreateBindableProperties);
			}
		}
	}

	protected BaseCreateMethodsFix createFix(final JSClass jsClass)
	{
		if(mode == GenerationMode.GETTERS_AND_SETTERS)
		{
			return new BaseCreateMethodsFix<JSVariable>(jsClass)
			{
				private boolean toCreateBindableProperties = myCreateBindableProperties != null ? myCreateBindableProperties.isSelected() : ApplicationManager
						.getApplication().isUnitTestMode();
				final MyBaseCreateMethodsFix generateGetterFix = new MyBaseCreateMethodsFix(GenerationMode.GETTERS, jsClass, toCreateBindableProperties);
				final MyBaseCreateMethodsFix generateSetterFix = new MyBaseCreateMethodsFix(GenerationMode.SETTERS, jsClass, toCreateBindableProperties);

				@Override
				public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
				{
					evalAnchor(editor, file);

					for(JSVariable e : getElementsToProcess())
					{
						anchor = doAddOneMethod(project, generateGetterFix.buildFunctionText(e), anchor);
						anchor = doAddOneMethod(project, generateSetterFix.buildFunctionText(e), anchor);
					}
				}
			};
		}
		else if(mode == GenerationMode.CONSTRUCTOR)
		{
			return new BaseCreateMethodsFix<JSVariable>(jsClass)
			{
				@Override
				public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
				{
					final JSCodeStyleSettings codeStyleSettings = CodeStyleSettingsManager.getSettings(project).getCustomSettings(JSCodeStyleSettings.class);
					evalAnchor(editor, file);
					@NonNls String functionText = "public function " + jsClass.getName() + "(";
					@NonNls String initialization = "";
					boolean first = true;
					final String semicolon = JSChangeUtil.getSemicolon(project);

					Set<JSVariable> toProcess = getElementsToProcess();
					Iterator<JSVariable> variableIterator = toProcess.iterator();
					boolean hadSuperClassConstructorInitializationBefore = false;

					while(variableIterator.hasNext())
					{
						JSVariable var = variableIterator.next();
						if(!first)
						{
							functionText += ", ";
						}

						first = false;

						final String name = var.getName();
						String parameterName = transformVarNameToAccessorName(name, codeStyleSettings);

						final String typeString = var.getTypeString();
						functionText += parameterName + (typeString != null ? ":" + typeString : "");

						if(JSResolveUtil.findParent(var) == jsClass)
						{
							if(hadSuperClassConstructorInitializationBefore)
							{
								initialization += ")" + semicolon + "\n";
								hadSuperClassConstructorInitializationBefore = false;
							}
							initialization += (parameterName.equals(name) ? "this." : "") + name + " = " + parameterName + semicolon + "\n";
						}
						else
						{
							if(hadSuperClassConstructorInitializationBefore)
							{
								initialization += ", ";
							}
							else
							{
								initialization += "super(";
							}
							initialization += parameterName;
							hadSuperClassConstructorInitializationBefore = true;
						}
					}

					if(hadSuperClassConstructorInitializationBefore)
					{
						initialization += ")" + semicolon + "\n";
					}
					functionText += ") {\n";
					functionText += initialization;
					functionText += "}";
					doAddOneMethod(project, functionText, anchor);
				}

				@Override
				public Set<JSVariable> getElementsToProcess()
				{
					LinkedHashSet<JSVariable> vars = new LinkedHashSet<JSVariable>();
					JSFunction nontrivialSuperClassConstructor = JSAnnotatingVisitor.getNontrivialSuperClassConstructor(jsClass);

					if(nontrivialSuperClassConstructor != null)
					{
						vars.addAll(Arrays.asList(nontrivialSuperClassConstructor.getParameterList().getParameters()));
					}
					vars.addAll(super.getElementsToProcess());
					return vars;
				}
			};
		}
		else if(mode == GenerationMode.TOSTRING)
		{
			return new BaseCreateMethodsFix<JSVariable>(jsClass)
			{
				@Override
				public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
				{
					evalAnchor(editor, file);

					final boolean[] needOverride = new boolean[1];
					JSResolveUtil.processOverrides(jsClass, new JSResolveUtil.OverrideHandler()
					{
						public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
						{
							needOverride[0] = !"Object".equals(className);
							return false;
						}
					}, "toString", null, myJsClass);

					@NonNls String functionText = "public " + (needOverride[0] ? "override " : "") + "function toString():String {\nreturn " +
							(needOverride[0] ? "super.toString() + \"" : "\"" + jsClass.getName()) + "{";
					final String semicolon = JSChangeUtil.getSemicolon(project);

					boolean first = true;

					for(JSVariable var : getElementsToProcess())
					{
						if(!first)
						{
							functionText += " + \",";
						}
						first = false;

						functionText += var.getName() + "=\" + String(" + var.getName() + ")";
					}

					functionText += "+\"}\"" + semicolon + "\n}";
					doAddOneMethod(project, functionText, anchor);
				}
			};
		}

		return new MyBaseCreateMethodsFix(mode, jsClass, myCreateBindableProperties != null ? myCreateBindableProperties.isSelected() : false);
	}


	protected void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates)
	{
		final LinkedHashMap<String, JSNamedElement> candidatesMap = new LinkedHashMap<String, JSNamedElement>();
		final JSCodeStyleSettings codeStyleSettings = CodeStyleSettingsManager.getSettings(clazz.getProject()).getCustomSettings(JSCodeStyleSettings
				.class);
		final ResolveProcessor processor = new ResolveProcessor(null)
		{
			{
				setToProcessMembers(true);
				setToProcessHierarchy(false);
				setLocalResolve(true);
			}

			@Override
			public boolean execute(final PsiElement element, final ResolveState state)
			{
				final JSNamedElement namedElement = (JSNamedElement) element;
				if(!(element instanceof JSVariable))
				{
					if(element instanceof JSFunction)
					{
						final JSFunction function = (JSFunction) element;
						if(mode == GenerationMode.GETTERS && function.isGetProperty() || mode == GenerationMode.SETTERS && function.isSetProperty())
						{
							candidatesMap.put(function.getName(), function);
						}

					}
					return true;
				}
				else if(((JSVariable) element).isConst())
				{
					return true;
				}

				final String name = namedElement.getName();
				final String accessorName = transformVarNameToAccessorName(name, codeStyleSettings);
				if(/*!name.equals(accessorName) &&*/ !candidatesMap.containsKey(accessorName))
				{
					candidatesMap.put(accessorName, namedElement);
				}
				return true;
			}
		};

		clazz.processDeclarations(processor, ResolveState.initial(), clazz, clazz);
		for(JSNamedElement n : candidatesMap.values())
		{
			if(n instanceof JSVariable)
			{
				candidates.add(new JSNamedElementNode(n));
			}
		}
	}

	private static class MyBaseCreateMethodsFix extends BaseCreateMethodsFix<JSVariable>
	{
		private GenerationMode myMode;
		private JSCodeStyleSettings codeStyleSettings;
		private boolean bindableProperties;
		private static final String PARAMETER_NAME = "value";

		public MyBaseCreateMethodsFix(final GenerationMode mode, final JSClass jsClass, boolean _bindableProperties)
		{
			super(jsClass);
			this.myMode = mode;
			codeStyleSettings = CodeStyleSettingsManager.getSettings(jsClass.getProject()).getCustomSettings(JSCodeStyleSettings.class);
			bindableProperties = _bindableProperties;
		}

		protected String buildFunctionBodyText(final String retType, final JSParameterList parameterList, final JSVariable func)
		{
			final String semicolon = codeStyleSettings.USE_SEMICOLON_AFTER_STATEMENT ? ";" : "";
			String varName = func.getName();
			if(myMode == GenerationMode.SETTERS)
			{
				String checkNeedEvent = "";
				String dispatchEvent = "";
				if(bindableProperties)
				{
					String eventName = getEventName(transformVarNameToAccessorName(varName, codeStyleSettings));
					dispatchEvent = "\ndispatchEvent(new Event(\"" + eventName + "\"))" + semicolon;
					checkNeedEvent = "if(" + varName + "==" + PARAMETER_NAME + ") return" + semicolon + "\n";
				}
				return "{\n" + checkNeedEvent + varName + "=" + PARAMETER_NAME + semicolon + dispatchEvent + "\n}";
			}
			else if(myMode == GenerationMode.GETTERS)
			{
				return "{\nreturn " + varName + semicolon + "\n}";
			}
			return " {}";
		}

		protected String buildFunctionAttrText(final String attrText, final JSAttributeList attributeList, final JSVariable function)
		{
			String baseText = "public" + (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) ? " static" : "");

			if(bindableProperties && myMode == GenerationMode.GETTERS)
			{
				baseText = "[Bindable(event=\"" + getEventName(transformVarNameToAccessorName(function.getName(), codeStyleSettings)) + "\")]\n" + baseText;
			}

			return baseText;
		}

		private static String getEventName(String name)
		{
			return name + "Changed";
		}

		@Override
		protected String buildFunctionKind(final JSVariable fun)
		{
			if(myMode == GenerationMode.GETTERS)
			{
				return "get ";
			}
			if(myMode == GenerationMode.SETTERS)
			{
				return "set ";
			}
			return super.buildFunctionKind(fun);
		}

		protected String buildReturnType(final String typeString)
		{
			if(myMode == GenerationMode.SETTERS)
			{
				return "void";
			}
			return super.buildReturnType(typeString);
		}

		@Override
		protected String buildName(final JSVariable fun)
		{
			return transformVarNameToAccessorName(super.buildName(fun), codeStyleSettings);
		}

		protected String buildParameterList(final JSParameterList parameterList, final JSVariable fun)
		{
			if(myMode == GenerationMode.SETTERS)
			{
				final String s = fun.getTypeString();
				return "(" + PARAMETER_NAME + (s != null ? ":" + s : "") + ")";
			}
			return (parameterList != null ? parameterList.getText() : "()");
		}
	}

	private static String transformVarNameToAccessorName(String s, final JSCodeStyleSettings codeStyleSettings)
	{
		if(StringUtil.startsWith(s, codeStyleSettings.FIELD_PREFIX))
		{
			s = s.substring(codeStyleSettings.FIELD_PREFIX.length());
		}
		s = codeStyleSettings.PROPERTY_PREFIX + s;
		return s;
	}

	protected boolean canHaveEmptySelectedElements()
	{
		return mode == GenerationMode.CONSTRUCTOR;
	}
}

