package consulo.javascript.lang.viewProvider;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.file.FileViewProvider;
import consulo.language.file.LanguageFileViewProviderFactory;
import consulo.language.impl.file.SingleRootFileViewProvider;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiManager;
import consulo.util.lang.reflect.ReflectionUtil;
import consulo.virtualFileSystem.VirtualFile;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
@ExtensionImpl
public class JSFileViewProviderFactory implements LanguageFileViewProviderFactory {
    private static class JSFileViewProvider extends SingleRootFileViewProvider {
        private JSFileViewProvider(
            @Nonnull PsiManager manager,
            @Nonnull VirtualFile virtualFile,
            boolean eventSystemEnabled,
            @Nonnull Language language
        ) {
            super(manager, virtualFile, eventSystemEnabled, language);
        }

        @Override
        @RequiredReadAction
        public PsiElement findElementAt(int offset, @Nonnull Class<? extends Language> lang) {
            PsiElement elementAt = findElementAt(offset);
            if (elementAt != null && ReflectionUtil.isAssignable(lang, elementAt.getLanguage().getClass())) {
                return elementAt;
            }
            return null;
        }
    }

    @Override
    public FileViewProvider createFileViewProvider(
        @Nonnull VirtualFile virtualFile,
        Language language,
        @Nonnull PsiManager psiManager,
        boolean eventSystemEnabled
    ) {
        return new JSFileViewProvider(psiManager, virtualFile, eventSystemEnabled, language);
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
