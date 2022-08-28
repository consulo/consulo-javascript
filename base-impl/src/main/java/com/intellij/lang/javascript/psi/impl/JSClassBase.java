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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.application.util.*;
import consulo.javascript.impl.language.psi.JSStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.util.collection.ArrayFactory;
import consulo.util.collection.ArrayUtil;
import consulo.util.dataholder.Key;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

/**
 * @author Maxim.Mossienko
 */
public abstract class JSClassBase extends JSStubElementImpl<JSClassStub> implements JSClass
{
	private volatile Map<String, Object> myName2FunctionMap;
	private volatile Map<String, JSVariable> myName2FieldsMap;
	private static Key<ParameterizedCachedValue<List<JSClass>, Object>> ourImplementsListCacheKey = Key.create("implements.list.cache");
	private static Key<ParameterizedCachedValue<List<JSClass>, Object>> ourExtendsListCacheKey = Key.create("implements.list.cache");
	private static UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase, Object> ourImplementsListCache = new
			ClassesUserDataCache();
	private static UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase,
			Object> ourExtendsListCache = new ExtendsClassesUserDataCache();

	protected JSClassBase(final ASTNode node)
	{
		super(node);
	}

	public JSClassBase(final JSClassStub stub, final JSStubElementType<JSClassStub, JSClass> aClass)
	{
		super(stub, aClass);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSClass(this);
	}

	@Override
	public void subtreeChanged()
	{
		dropCaches();

		super.subtreeChanged();
	}

	@Override
	protected Object clone()
	{
		final JSClassBase o = (JSClassBase) super.clone();
		o.dropCaches();
		return o;
	}

	private void dropCaches()
	{
		synchronized(this)
		{
			myName2FunctionMap = null;
			myName2FieldsMap = null;
		}
	}

	@Override
	public JSFunction[] getFunctions()
	{
		final JSClassStub classStub = getStub();
		if(classStub != null)
		{
			return getStubChildrenByType(classStub, JSElementTypes.FUNCTION_DECLARATION, JSFunction.ARRAY_FACTORY);
		}
		else
		{
			final List<JSFunction> functions = new ArrayList<JSFunction>();
			processDeclarations(new PsiScopeProcessor()
			{
				@Override
				public boolean execute(final PsiElement element, final ResolveState state)
				{
					if(element instanceof JSFunction)
					{
						functions.add((JSFunction) element);
					}
					return true;
				}

				@Override
				public <T> T getHint(final Key<T> hintClass)
				{
					return null;
				}

				@Override
				public void handleEvent(final Event event, final Object associated)
				{
				}
			}, ResolveState.initial(), this, this);
			return functions.toArray(JSFunction.EMPTY_ARRAY);
		}
	}

	@Override
	public JSFunction findFunctionByName(String functionName)
	{
		if(functionName == null)
		{
			return null;
		}
		final Map<String, Object> name2FunctionMap = initFunctions();
		final Object o = name2FunctionMap.get(functionName);
		if(o instanceof JSFunction)
		{
			return (JSFunction) o;
		}
		else if(o instanceof JSFunction[])
		{
			return ((JSFunction[]) o)[0];
		}
		return null;
	}

	@Override
	public JSVariable[] getFields()
	{
		final JSClassStub classStub = getStub();
		final List<JSVariable> vars = new ArrayList<JSVariable>(3);

		if(classStub != null)
		{
			for(JSVarStatement var : getStubChildrenByType(classStub, JSElementTypes.VAR_STATEMENT, new ArrayFactory<JSVarStatement>()
			{
				@Override
				public JSVarStatement[] create(final int count)
				{
					return new JSVarStatement[count];
				}
			}))
			{
				vars.addAll(Arrays.asList(var.getVariables()));
			}
		}
		else
		{
			processDeclarations(new PsiScopeProcessor()
			{
				@Override
				public boolean execute(final PsiElement element, final ResolveState state)
				{
					if(element instanceof JSVariable)
					{
						vars.add((JSVariable) element);
					}
					return true;
				}

				@Override
				public <T> T getHint(final Key<T> hintClass)
				{
					return null;
				}

				@Override
				public void handleEvent(final Event event, final Object associated)
				{
				}
			}, ResolveState.initial(), this, this);
		}
		return vars.toArray(JSVariable.EMPTY_ARRAY);
	}

	@Override
	public JSVariable findFieldByName(String name)
	{
		return initFields().get(name);
	}

	private
	@Nonnull
	Map<String, JSVariable> initFields()
	{
		Map<String, JSVariable> name2FieldsMap = myName2FieldsMap;

		if(name2FieldsMap == null)
		{
			synchronized(this)
			{
				name2FieldsMap = myName2FieldsMap;

				if(name2FieldsMap == null)
				{
					name2FieldsMap = new HashMap<String, JSVariable>();

					for(JSVariable variable : getFields())
					{
						final String name = variable.getName();

						if(name != null)
						{
							name2FieldsMap.put(name, variable);
						}
					}
					myName2FieldsMap = name2FieldsMap;
				}
			}
		}
		return name2FieldsMap;
	}

	private
	@Nonnull
	Map<String, Object> initFunctions()
	{
		Map<String, Object> name2FunctionMap = myName2FunctionMap;

		if(name2FunctionMap == null)
		{
			synchronized(this)
			{
				name2FunctionMap = myName2FunctionMap;

				if(name2FunctionMap == null)
				{
					name2FunctionMap = new HashMap<String, Object>();

					for(JSFunction function : getFunctions())
					{
						final String name = function.getName();

						if(name != null)
						{
							final Object o = name2FunctionMap.get(name);
							if(o == null)
							{
								name2FunctionMap.put(name, function);
							}
							else if(o instanceof JSFunction)
							{
								name2FunctionMap.put(name, new JSFunction[]{
										(JSFunction) o,
										function
								});
							}
							else if(o instanceof JSFunction[])
							{
								name2FunctionMap.put(name, ArrayUtil.append((JSFunction[]) o, function));
							}
						}
					}
					myName2FunctionMap = name2FunctionMap;
				}
			}
		}
		return name2FunctionMap;
	}

	@Override
	public JSFunction findFunctionByNameAndKind(final String name, JSFunction.FunctionKind kind)
	{
		if(name == null)
		{
			return null;
		}
		Map<String, Object> name2FunctionMap = initFunctions();
		final Object o = name2FunctionMap.get(name);

		if(o instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) o;
			return function.getKind() == kind ? function : null;
		}
		else if(o instanceof JSFunction[])
		{
			for(JSFunction fun : (JSFunction[]) o)
			{
				if(fun.getKind() == kind)
				{
					return fun;
				}
			}
		}
		return null;
	}

	@Override
	public JSClass[] getSupers()
	{
		List<JSClass> superClasses = new ArrayList<JSClass>(getClassesFromReferenceList(getExtendsList(), JSElementTypes.EXTENDS_LIST));
		superClasses.addAll(getClassesFromReferenceList(getImplementsList(), JSElementTypes.IMPLEMENTS_LIST));
		return superClasses.toArray(new JSClass[superClasses.size()]);
	}

	private List<JSClass> getClassesFromReferenceList(final @Nullable JSReferenceList extendsList, @Nonnull IElementType type)
	{
		final PsiElement element = extendsList != null ? extendsList : this;

		if(type == JSElementTypes.EXTENDS_LIST)
		{
			return ourExtendsListCache.get(ourExtendsListCacheKey, this, extendsList).getValue(element);
		}
		else
		{
			return ourImplementsListCache.get(ourImplementsListCacheKey, this, extendsList).getValue(element);
		}
	}

	@Override
	public boolean processDeclarations(@Nonnull final PsiScopeProcessor processor, @Nonnull final ResolveState substitutor,
			final PsiElement lastParent, @Nonnull final PsiElement place)
	{
		final ResolveProcessor resolveProcessor = processor instanceof ResolveProcessor ? (ResolveProcessor) processor : null;
		final boolean toProcessClass = resolveProcessor != null && resolveProcessor.isTypeContext();

		if(toProcessClass)
		{
			if(!processor.execute(this, null))
			{
				return false;
			}
		}

		if(lastParent == null)
		{
			return true;
		}

		processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
		final boolean toProcessMembers = (resolveProcessor == null || (!resolveProcessor.isToSkipClassDeclarationOnce() && resolveProcessor
				.isToProcessMembers()));
		if(toProcessMembers)
		{
			if(!processMembers(processor, substitutor, lastParent, place))
			{
				return false;
			}
		}
		else
		{
			resolveProcessor.setToSkipClassDeclarationsOnce(false);
		}

		final boolean toProcessInHierarchy = processor instanceof ResolveProcessor && ((ResolveProcessor) processor).isToProcessHierarchy();

		if(!toProcessInHierarchy || ((ResolveProcessor) processor).checkVisited(getQualifiedName()))
		{
			return true;
		}

		for(JSClass clazz : getSuperClasses())
		{
			if(!clazz.processDeclarations(processor, ResolveState.initial(), lastParent, place))
			{
				return false;
			}
		}

		return true;
	}

	protected abstract boolean processMembers(final PsiScopeProcessor processor, final ResolveState substitutor, final PsiElement lastParent,
			final PsiElement place);

	@Override
	public JSClass[] getSuperClasses()
	{
		final JSReferenceList extendsList = getExtendsList();
		final List<JSClass> supers = getClassesFromReferenceList(extendsList, JSElementTypes.EXTENDS_LIST);
		return supers.toArray(new JSClass[supers.size()]);
	}

	public static PsiElement findClassFromNamespace(final String qname, PsiElement context)
	{
		PsiElement realClazz = null;
		final PsiElement clazz = JSResolveUtil.findClassByQName(qname, context);

		realClazz = clazz;
		return realClazz;
	}

	@Override
	public JSClass[] getImplementedInterfaces()
	{
		final JSReferenceList implementsList = getImplementsList();
		if(implementsList == null)
		{
			return JSClass.EMPTY_ARRAY;
		}
		final List<JSClass> classes = getClassesFromReferenceList(implementsList, JSElementTypes.IMPLEMENTS_LIST);
		return classes.toArray(new JSClass[classes.size()]);
	}

	private static class ExtendsClassesUserDataCache extends ClassesUserDataCache
	{
		@Override
		protected List<JSClass> doCompute(final Object extendsList)
		{
			if(extendsList instanceof JSClass)
			{
				final JSClass jsClass = (JSClass) extendsList;

				final ArrayList<JSClass> supers = new ArrayList<JSClass>(1);
				if(!"Object".equals(jsClass.getQualifiedName()))
				{
					final PsiElement element = findClassFromNamespace("Object", jsClass);
					if(element instanceof JSClass)
					{
						supers.add((JSClass) element);
					}
				}

				return supers;
			}
			return super.doCompute(extendsList);
		}
	}

	private static class ClassesUserDataCache extends UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase, Object>
	{
		@Override
		protected ParameterizedCachedValue<List<JSClass>, Object> compute(final JSClassBase jsClassBase, final Object p)
		{
			return CachedValuesManager.getManager(jsClassBase.getProject()).createParameterizedCachedValue(new
																												   ParameterizedCachedValueProvider<List<JSClass>, Object>()
			{
				@Override
				public CachedValueProvider.Result<List<JSClass>> compute(Object list)
				{
					return new CachedValueProvider.Result<List<JSClass>>(doCompute(list), PsiModificationTracker.MODIFICATION_COUNT);
				}
			}, false);
		}

		protected List<JSClass> doCompute(final Object object)
		{
			if(object instanceof JSClass)
			{
				return Collections.emptyList();
			}

			final ArrayList<JSClass> supers = new ArrayList<JSClass>(1);
			final JSReferenceList extendsList = (JSReferenceList) object;

			for(String refText : extendsList.getReferenceTexts())
			{
				refText = JSImportHandlingUtil.resolveTypeName(refText, extendsList.getParent());
				final PsiElement element = findClassFromNamespace(refText, extendsList.getParent());
				if(element instanceof JSClass)
				{
					supers.add((JSClass) element);
				}
			}
			return supers;
		}
	}

	private static <E extends PsiElement> E[] getStubChildrenByType(JSClassStub stub, final IElementType elementType, ArrayFactory<E> f)
	{
		assert JSElementTypes.INCLUDE_DIRECTIVE != elementType;

		ArrayList<E> result = new ArrayList<E>(Arrays.asList(stub.getChildrenByType(elementType, f)));
		JSIncludeDirective[] includes = stub.getChildrenByType(JSElementTypes.INCLUDE_DIRECTIVE, new ArrayFactory<JSIncludeDirective>()
		{
			@Override
			public JSIncludeDirective[] create(final int count)
			{
				return new JSIncludeDirective[count];
			}
		});
		Collection<JSFile> visited = new HashSet<JSFile>();
		TokenSet filter = TokenSet.create(JSElementTypes.INCLUDE_DIRECTIVE, elementType);
		for(JSIncludeDirective include : includes)
		{
			PsiFile file = include.resolveFile();
			if(file instanceof JSFile)
			{
				process(filter, (JSFile) file, result, visited);
			}
		}
		return result.toArray(f.create(result.size()));
	}

	private static <E extends PsiElement> void process(TokenSet filter, final JSFile file, final ArrayList<E> result, final Collection<JSFile> visited)
	{
		if(visited.contains(file))
		{
			return;
		}
		visited.add(file);
		for(PsiElement element : JSResolveUtil.getStubbedChildren(file, filter))
		{
			if(element instanceof JSIncludeDirective)
			{
				PsiFile includedFile = ((JSIncludeDirective) element).resolveFile();
				if(includedFile instanceof JSFile)
				{
					process(filter, (JSFile) includedFile, result, visited);
				}
			}
			else
			{
				result.add((E) element);
			}
		}
	}


}
