package consulo.javascript.regexp.impl;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.lang.psi.impl.JSRegExpLiteralExpressionImpl;
import org.intellij.lang.regexp.DefaultRegExpPropertiesProvider;
import org.intellij.lang.regexp.RegExpLanguageHost;
import org.intellij.lang.regexp.psi.RegExpChar;
import org.intellij.lang.regexp.psi.RegExpGroup;
import org.intellij.lang.regexp.psi.RegExpNamedGroupRef;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 21/12/2021
 */
@ExtensionImpl
public class JavaScriptRegExpLiteralLanguageHost implements RegExpLanguageHost
{
	@Nonnull
	@Override
	public Class getHostClass()
	{
		return JSRegExpLiteralExpressionImpl.class;
	}

	@Override
	public boolean characterNeedsEscaping(char c)
	{
		if(c == '/')
		{
			return true;
		}
		return false;
	}

	@Override
	public boolean supportsPerl5EmbeddedComments()
	{
		return false;
	}

	@Override
	public boolean supportsPossessiveQuantifiers()
	{
		return false;
	}

	@Override
	public boolean supportsPythonConditionalRefs()
	{
		return false;
	}

	@Override
	public boolean supportsNamedGroupSyntax(RegExpGroup group)
	{
		return false;
	}

	@Override
	public boolean supportsNamedGroupRefSyntax(RegExpNamedGroupRef ref)
	{
		return false;
	}

	@Override
	public boolean supportsExtendedHexCharacter(RegExpChar regExpChar)
	{
		return false;
	}

	@Override
	public boolean isValidCategory(@Nonnull String category)
	{
		return DefaultRegExpPropertiesProvider.getInstance().isValidCategory(category);
	}

	@Nonnull
	@Override
	public String[][] getAllKnownProperties()
	{
		return DefaultRegExpPropertiesProvider.getInstance().getAllKnownProperties();
	}

	@Nullable
	@Override
	public String getPropertyDescription(@Nullable String name)
	{
		return DefaultRegExpPropertiesProvider.getInstance().getPropertyDescription(name);
	}

	@Nonnull
	@Override
	public String[][] getKnownCharacterClasses()
	{
		return DefaultRegExpPropertiesProvider.getInstance().getKnownCharacterClasses();
	}
}
