package com.intellij.lang.javascript.index;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSNameIndex;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.Processor;
import com.intellij.util.indexing.FileBasedIndex;

/**
 * @author maxim
 */
public class JavaScriptClassContributor implements ChooseByNameContributor
{
	public String[] getNames(Project project, boolean includeNonProjectItems)
	{
		final Set<String> result = new HashSet<String>();

		result.addAll(StubIndex.getInstance().getAllKeys(JSNameIndex.KEY, project));

		FileBasedIndex.getInstance().processAllKeys(FilenameIndex.NAME, new Processor<String>()
		{
			public boolean process(String s)
			{
				if(JavaScriptSupportLoader.isFlexMxmFile(s))
				{
					result.add(FileUtil.getNameWithoutExtension(s));
				}
				return true;
			}
		}, project);
		return result.toArray(new String[result.size()]);
	}

	public NavigationItem[] getItemsByName(String name, final String pattern, Project project, boolean includeNonProjectItems)
	{
		GlobalSearchScope scope = includeNonProjectItems ? ProjectScope.getAllScope(project) : ProjectScope.getProjectScope(project);
		Collection<JSQualifiedNamedElement> result = JSResolveUtil.findElementsByName(name, project, scope);
		return result.toArray(new NavigationItem[result.size()]);
	}

}
