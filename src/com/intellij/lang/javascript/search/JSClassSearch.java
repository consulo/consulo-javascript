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

package com.intellij.lang.javascript.search;

import gnu.trove.THashSet;

import java.util.Collection;
import java.util.Set;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.stubs.JSImplementedInterfacesIndex;
import com.intellij.lang.javascript.psi.stubs.JSSuperClassIndex;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexKey;
import com.intellij.util.Processor;
import com.intellij.util.Query;
import com.intellij.util.QueryExecutor;
import com.intellij.util.QueryFactory;

public abstract class JSClassSearch implements QueryExecutor<JSClass, JSClassSearch.SearchParameters>
{
	public static class SearchParameters
	{
		private final JSClass myClass;
		private final boolean myCheckDeepInheritance;
		private final GlobalSearchScope myScope;

		public SearchParameters(final JSClass anClass, final boolean checkDeepInheritance, GlobalSearchScope scope)
		{
			myClass = anClass;
			myCheckDeepInheritance = checkDeepInheritance;
			myScope = scope;
		}

		public JSClass getTargetClass()
		{
			return myClass;
		}

		public boolean isCheckDeepInheritance()
		{
			return myCheckDeepInheritance;
		}

		public GlobalSearchScope getScope()
		{
			return myScope;
		}
	}

	public static Query<JSClass> searchClassInheritors(final JSClass superClass, final boolean checkDeepInheritance)
	{
		final SearchParameters parameters = new SearchParameters(superClass, checkDeepInheritance, getUseScope(superClass));
		return CLASS_INHERITORS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
	}

	private static GlobalSearchScope getUseScope(JSClass superClass)
	{
		SearchScope searchScope = superClass.getUseScope();
		return (GlobalSearchScope) searchScope;
	}

	public static Query<JSClass> searchInterfaceImplementations(final JSClass superClass, final boolean checkDeepInheritance)
	{
		final SearchParameters parameters = new SearchParameters(superClass, checkDeepInheritance, getUseScope(superClass));
		return INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
	}

	// implementation
	private static final QueryFactory<JSClass, SearchParameters> INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY = new QueryFactory<JSClass,
			SearchParameters>();
	private static final QueryFactory<JSClass, SearchParameters> CLASS_INHERITORS_QUERY_FACTORY = new QueryFactory<JSClass, SearchParameters>();
	private static final JSClassSearch OUR_CLASS_SEARCH_EXECUTOR;

	static
	{
		INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY.registerExecutor(new JSClassSearch()
		{
			@Override
			protected StubIndexKey<String, JSClass> getIndexKey()
			{
				return JSImplementedInterfacesIndex.KEY;
			}

			@Override
			protected JSClass[] getSupers(final JSClass candidate)
			{
				return candidate.getImplementedInterfaces();
			}

			@Override
			public boolean execute(final SearchParameters queryParameters, Processor<JSClass> consumer)
			{
				final THashSet<JSClass> visited = new THashSet<JSClass>();         // no abstract classes in ActionScript !

				if(queryParameters.isCheckDeepInheritance())
				{
					final Processor<JSClass> consumerCopy = consumer;
					consumer = new Processor<JSClass>()
					{
						@Override
						public boolean process(JSClass jsClass)
						{
							return consumerCopy.process(jsClass) && OUR_CLASS_SEARCH_EXECUTOR.processDirectInheritors(jsClass, this, false, visited,
									queryParameters.getScope());
						}
					};
				}

				final Processor<JSClass> consumerToUse = consumer;
				final boolean b = processDirectInheritors(queryParameters.getTargetClass(), consumerToUse, queryParameters.isCheckDeepInheritance(), visited,
						queryParameters.getScope());
				if(b)
				{
					return searchClassInheritors(queryParameters.getTargetClass(), queryParameters.isCheckDeepInheritance()).forEach(new Processor<JSClass>()
					{
						@Override
						public boolean process(final JSClass jsClass)
						{
							return processDirectInheritors(jsClass, consumerToUse, queryParameters.isCheckDeepInheritance(), visited, queryParameters.getScope());
						}
					});
				}
				return b;
			}

			@Override
			protected Collection<JSClass> getInheritors(JSClassInheritorsProvider provider, String parentName, Project project, GlobalSearchScope scope)
			{
				return provider.getImplementingClasses(parentName, project, scope);
			}
		});

		CLASS_INHERITORS_QUERY_FACTORY.registerExecutor(OUR_CLASS_SEARCH_EXECUTOR = new JSClassSearch()
		{
			@Override
			protected StubIndexKey<String, JSClass> getIndexKey()
			{
				return JSSuperClassIndex.KEY;
			}

			@Override
			protected JSClass[] getSupers(final JSClass candidate)
			{
				return candidate.getSuperClasses();
			}

			@Override
			protected Collection<JSClass> getInheritors(JSClassInheritorsProvider provider, String parentName, Project project, GlobalSearchScope scope)
			{
				return provider.getExtendingClasses(parentName, project, scope);
			}

		});
	}


	@Override
	public boolean execute(final SearchParameters queryParameters, final Processor<JSClass> consumer)
	{
		return processDirectInheritors(queryParameters.getTargetClass(), consumer, queryParameters.isCheckDeepInheritance(), null,
				queryParameters.getScope());
	}

	protected boolean processDirectInheritors(final JSClass superClass, final Processor<JSClass> consumer, final boolean checkDeep,
			Set<JSClass> processed, final GlobalSearchScope scope)
	{
		if(processed != null)
		{
			if(processed.contains(superClass))
			{
				return true;
			}
		}
		else
		{
			processed = new THashSet<JSClass>();
		}

		processed.add(superClass);
		Project project = superClass.getProject();
		final String name = superClass.getName();
		if(name == null)
		{
			return true;
		}

		final Set<JSClass> temp = processed;
		Processor<JSClass> processor = new Processor<JSClass>()
		{
			@Override
			public boolean process(JSClass candidate)
			{
				final JSClass[] classes = getSupers(candidate);
				if(classes != null)
				{
					for(JSClass superClassCandidate : classes)
					{
						if(superClassCandidate.isEquivalentTo(superClass))
						{
							if(!consumer.process(candidate))
							{
								return false;
							}
							if(checkDeep && !processDirectInheritors(candidate, consumer, checkDeep, temp, scope))
							{
								return false;
							}
						}
					}
				}

				return true;
			}
		};

		if(!StubIndex.getInstance().processElements(getIndexKey(), name, project, scope, JSClass.class, processor))
		{
			return false;
		}

		for(JSClassInheritorsProvider provider : Extensions.getExtensions(JSClassInheritorsProvider.EP_NAME))
		{
			Collection<JSClass> inheritors = getInheritors(provider, name, project, scope);
			for(JSClass inheritor : inheritors)
			{
				if(!processor.process(inheritor))
				{
					return false;
				}
			}
		}
		return true;
	}

	protected abstract Collection<JSClass> getInheritors(JSClassInheritorsProvider provider, String parentName, Project project,
			GlobalSearchScope scope);

	protected abstract StubIndexKey<String, JSClass> getIndexKey();

	protected abstract JSClass[] getSupers(final JSClass candidate);
}