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

package consulo.json.validation.descriptionByAnotherPsiElement;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import org.jdom.Element;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.pointers.VirtualFilePointer;
import com.intellij.openapi.vfs.pointers.VirtualFilePointerManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import com.intellij.util.containers.ContainerUtil;
import consulo.disposer.Disposable;
import consulo.disposer.Disposer;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
@Singleton
@State(name = "JSONDescriptionByAnotherPsiElementService",storages = @Storage(StoragePathMacros.WORKSPACE_FILE))
public class DescriptionByAnotherPsiElementService implements PersistentStateComponent<Element>, Disposable
{
	private static class Info implements Disposable
	{
		private final VirtualFilePointer myVirtualFilePointer;
		private SmartPsiElementPointer<? extends PsiElement> myElementPointer;
		private final DescriptionByAnotherPsiElementProvider myProvider;

		private final String myId;
		private String myPsiElementId;
		private Project myProject;

		public Info(@Nonnull Project project, @Nonnull VirtualFile virtualFile, @Nonnull PsiElement element, @Nonnull DescriptionByAnotherPsiElementProvider<?> provider)
		{
			myProject = project;
			myId = provider.getId();
			myProvider = provider;
			myVirtualFilePointer = VirtualFilePointerManager.getInstance().create(virtualFile, this, null);
			myElementPointer = SmartPointerManager.getInstance(element.getProject()).createSmartPsiElementPointer(element);
		}

		public Info(@Nonnull Project project, @Nonnull String url, @Nonnull final String providerId, @Nonnull String psiElementId)
		{
			myProject = project;
			myVirtualFilePointer = VirtualFilePointerManager.getInstance().create(url, this, null);

			myProvider = ContainerUtil.find(DescriptionByAnotherPsiElementProvider.EP_NAME.getExtensions(), new Condition<DescriptionByAnotherPsiElementProvider<?>>()
			{
				@Override
				public boolean value(DescriptionByAnotherPsiElementProvider<?> psiElementProvider)
				{
					return psiElementProvider.getId().equals(providerId);
				}
			});

			myId = providerId;
			myPsiElementId = psiElementId;
		}

		public void tryToInit()
		{
			if(myProvider == null || myElementPointer != null)
			{
				return;
			}

			ApplicationManager.getApplication().runReadAction(new Runnable()
			{
				@Override
				public void run()
				{
					PsiElement psiElementById = myProvider.getPsiElementById(myPsiElementId, myProject);
					if(psiElementById == null)
					{
						return;
					}
					myElementPointer = SmartPointerManager.getInstance(myProject).createSmartPsiElementPointer(psiElementById);
				}
			});
		}

		@Nonnull
		public String getId()
		{
			if(myId != null)
			{
				return myId;
			}
			return myProvider.getId();
		}

		@Nonnull
		public String getUrl()
		{
			return myVirtualFilePointer.getUrl();
		}

		@Nullable
		@SuppressWarnings("unchecked")
		public String getPsiElementId()
		{
			if(myPsiElementId != null)
			{
				return myPsiElementId;
			}

			return ApplicationManager.getApplication().runReadAction(new Computable<String>()
			{
				@Override
				public String compute()
				{
					PsiElement element = myElementPointer.getElement();
					if(element == null)
					{
						return null;
					}
					return myProvider.getIdFromPsiElement(element);
				}
			});
		}

		@Nullable
		public PsiElement getPsiElement()
		{
			if(myElementPointer == null)
			{
				if(myProvider == null)
				{
					return null;
				}
				return ApplicationManager.getApplication().runReadAction(new Computable<PsiElement>()
				{
					@Override
					public PsiElement compute()
					{
						PsiElement element = myElementPointer == null ? null : myElementPointer.getElement();
						if(element == null)
						{
							return null;
						}
						return myProvider.getPsiElementById(myPsiElementId, myProject);
					}
				});
			}
			else
			{
				return myElementPointer.getElement();
			}
		}

		@Override
		public void dispose()
		{
		}
	}

	@Nonnull
	public static DescriptionByAnotherPsiElementService getInstance(@Nonnull Project project)
	{
		return ServiceManager.getService(project, DescriptionByAnotherPsiElementService.class);
	}

	private final Project myProject;
	private final List<Info> myRegisteredFiles = new ArrayList<Info>();

	@Inject
	public DescriptionByAnotherPsiElementService(Project project)
	{
		myProject = project;
	}

	public <T extends PsiElement> void registerFile(@Nonnull VirtualFile virtualFile, @Nonnull T element, @Nonnull DescriptionByAnotherPsiElementProvider<?> provider)
	{
		Info info = new Info(myProject, virtualFile, element, provider);
		Disposer.register(this, info);
		myRegisteredFiles.add(info);
	}


	public boolean removeFile(@Nonnull VirtualFile file)
	{
		Iterator<Info> iterator = myRegisteredFiles.iterator();
		while(iterator.hasNext())
		{
			Info info = iterator.next();
			if(file.equals(info.myVirtualFilePointer.getFile()))
			{
				iterator.remove();
				Disposer.dispose(info);
				return true;
			}
		}
		return false;
	}

	@Nullable
	public String getRegisteredPsiElementId(@Nonnull VirtualFile virtualFile)
	{
		for(Info registeredFile : myRegisteredFiles)
		{
			VirtualFile file = registeredFile.myVirtualFilePointer.getFile();
			if(virtualFile.equals(file))
			{
				return registeredFile.getPsiElementId();
			}
		}
		return null;
	}

	@Nonnull
	@SuppressWarnings("unchecked")
	public <T extends PsiElement> Pair<DescriptionByAnotherPsiElementProvider<T>, T> getRegisteredPsiElementInfo(@Nonnull VirtualFile virtualFile)
	{
		for(Info info : myRegisteredFiles)
		{
			VirtualFile file = info.myVirtualFilePointer.getFile();
			if(virtualFile.equals(file))
			{
				PsiElement psiElement = info.getPsiElement();
				if(psiElement == null)
				{
					return Pair.empty();
				}
				return new Pair.NonNull(info.myProvider, psiElement);
			}
		}
		return Pair.empty();
	}

	@Nullable
	@Override
	public Element getState()
	{
		Element stateElement = new Element("state");
		for(final Info registeredFile : myRegisteredFiles)
		{
			String psiElementId = registeredFile.getPsiElementId();
			if(psiElementId == null)
			{
				continue;
			}

			Element fileElement = new Element("file");
			fileElement.setAttribute("id", registeredFile.getId());
			fileElement.setAttribute("url", registeredFile.getUrl());
			fileElement.setAttribute("psiElementId", psiElementId);
			stateElement.addContent(fileElement);
		}
		return stateElement;
	}

	@Override
	public void loadState(Element state)
	{
		if(!myRegisteredFiles.isEmpty())
		{
			for(Info registeredFile : myRegisteredFiles)
			{
				Disposer.dispose(registeredFile);
			}
			myRegisteredFiles.clear();
		}

		for(Element element : state.getChildren("file"))
		{
			String id = element.getAttributeValue("id");
			String url = element.getAttributeValue("url");
			String psiElementId = element.getAttributeValue("psiElementId");

			final Info info = new Info(myProject, url, id, psiElementId);
			ApplicationManager.getApplication().runReadAction(new Runnable()
			{
				@Override
				public void run()
				{
					info.tryToInit();
				}
			});
			Disposer.register(this, info);
			myRegisteredFiles.add(info);
		}
	}

	@Override
	public void dispose()
	{
		myRegisteredFiles.clear();
	}
}
