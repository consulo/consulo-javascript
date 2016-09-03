package consulo.javascript.lang.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.impl.PsiBuilderAdapter;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class JavaScriptParserBuilder extends PsiBuilderAdapter
{
	public JavaScriptParserBuilder(PsiBuilder delegate)
	{
		super(delegate);
	}
}
