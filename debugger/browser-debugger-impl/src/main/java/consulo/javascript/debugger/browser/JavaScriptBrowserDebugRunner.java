package consulo.javascript.debugger.browser;

import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.runners.DefaultProgramRunner;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserDebugRunner extends DefaultProgramRunner
{
	@Nonnull
	@Override
	public String getRunnerId()
	{
		return "browser-javascript-debug";
	}

	@Override
	public boolean canRun(@Nonnull String executorId, @Nonnull RunProfile runProfile)
	{
		return  executorId.equals(DefaultDebugExecutor.EXECUTOR_ID) && runProfile instanceof JavaScriptBrowserConfiguration;
	}
}
