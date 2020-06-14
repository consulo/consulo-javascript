package consulo.javascript.lang;

import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.impl.FilePropertyPusher;
import com.intellij.openapi.roots.impl.PushedFilePropertiesUpdater;
import com.intellij.openapi.util.Conditions;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.FileAttribute;
import com.intellij.util.messages.MessageBus;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.module.extension.ModuleExtension;
import consulo.util.dataholder.Key;
import consulo.util.lang.ObjectUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

/**
 * @author VISTALL
 * @since 2020-06-14
 */
public class JavaScriptLanguageVersionPusher implements FilePropertyPusher<LanguageVersion>
{
	private static final FileAttribute ourFileAttribute = new FileAttribute("javascript-version", 2, false);

	private static final Key<Set<String>> ourChangedModulesKey = Key.create("javascript-change-marker");

	@Override
	public void initExtra(@Nonnull Project project, @Nonnull MessageBus messageBus)
	{
		project.getMessageBus().connect().subscribe(ModuleExtension.CHANGE_TOPIC, (oldExtension, newExtension) ->
		{
			if(oldExtension instanceof JavaScriptModuleExtension && newExtension instanceof JavaScriptModuleExtension)
			{
				if(((JavaScriptModuleExtension) oldExtension).getLanguageVersion() != ((JavaScriptModuleExtension) newExtension).getLanguageVersion())
				{
					addChanged(project, newExtension);
				}
			}
		});
	}

	private void addChanged(Project project, ModuleExtension<?> newExtension)
	{
		Set<String> changedModules = project.getUserData(ourChangedModulesKey);
		if(changedModules == null)
		{
			changedModules = new HashSet<>();
		}

		project.putUserData(ourChangedModulesKey, changedModules);

		changedModules.add(newExtension.getModule().getName());
	}

	@Nonnull
	@Override
	public Key<LanguageVersion> getFileDataKey()
	{
		return LanguageVersion.KEY;
	}

	@Override
	public boolean pushDirectoriesOnly()
	{
		return false;
	}

	@Nonnull
	@Override
	public LanguageVersion getDefaultValue()
	{
		return StandardJavaScriptVersions.getInstance().getDefaultVersion();
	}

	@Nullable
	@Override
	@RequiredReadAction
	public LanguageVersion getImmediateValue(@Nonnull Project project, @Nullable VirtualFile virtualFile)
	{
		if(virtualFile == null)
		{
			return getDefaultValue();
		}

		Module moduleForFile = ModuleUtilCore.findModuleForFile(virtualFile, project);
		return moduleForFile == null ? getDefaultValue() : getImmediateValue(moduleForFile);
	}

	@Nullable
	@Override
	@RequiredReadAction
	public LanguageVersion getImmediateValue(@Nonnull Module module)
	{
		JavaScriptModuleExtension<?> extension = ModuleUtilCore.getExtension(module, JavaScriptModuleExtension.class);
		if(extension != null)
		{
			return extension.getLanguageVersion();
		}
		return getDefaultValue();
	}

	@Override
	public boolean acceptsFile(@Nonnull VirtualFile virtualFile)
	{
		return virtualFile.getFileType() == JavaScriptFileType.INSTANCE;
	}

	@Override
	public boolean acceptsDirectory(@Nonnull VirtualFile virtualFile, @Nonnull Project project)
	{
		return false;
	}

	@Override
	public void persistAttribute(@Nonnull Project project, @Nonnull VirtualFile virtualFile, @Nonnull LanguageVersion newAttribute) throws IOException
	{
		DataInputStream inputStream = ourFileAttribute.readAttribute(virtualFile);
		if(inputStream != null)
		{
			try
			{
				LanguageVersion oldAttribute = read(inputStream);
				if(oldAttribute.equals(newAttribute) && oldAttribute != getDefaultValue())
				{
					return;
				}
			}
			catch(IOException e)
			{
				inputStream.close();
			}
		}

		if(newAttribute != getDefaultValue() || inputStream != null)
		{
			try (DataOutputStream oStream = ourFileAttribute.writeAttribute(virtualFile))
			{
				write(newAttribute, oStream);
			}

			PushedFilePropertiesUpdater.getInstance(project).filePropertiesChanged(virtualFile, Conditions.alwaysTrue());
		}
	}

	@Override
	public void afterRootsChanged(@Nonnull Project project)
	{
		Set<String> changedModules = project.getUserData(ourChangedModulesKey);
		if(changedModules == null)
		{
			return;
		}

		project.putUserData(ourChangedModulesKey, null);

		if(!changedModules.isEmpty())
		{
			PushedFilePropertiesUpdater.getInstance(project).pushAll(this);
		}
	}

	@Nonnull
	private LanguageVersion read(DataInputStream stream) throws IOException
	{
		String id = stream.readUTF();
		LanguageVersion version = JavaScriptLanguage.INSTANCE.getVersionById(id);
		return ObjectUtil.notNull(version, getDefaultValue());

	}

	private void write(LanguageVersion jsVersion, DataOutputStream stream) throws IOException
	{
		stream.writeUTF(jsVersion.getId());
	}
}
