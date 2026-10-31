package consulo.typescript.version;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFileTypeWithVersion;
import consulo.language.Language;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.version.LanguageVersion;
import consulo.language.version.LanguageVersionResolver;
import consulo.project.Project;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

/**
 * TypeScript versions. Without it the JavaScript resolver (base language) answers with a JavaScript version wherever
 * there is no .ts file: TypeScript embedded into HTML / Vue ({@code <script lang="ts">}), the default version.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class TypeScriptLanguageVersionResolver implements LanguageVersionResolver {
    @RequiredReadAction
    @Override
    public LanguageVersion getLanguageVersion(Language language, @Nullable PsiElement element) {
        PsiFile containingFile = element == null ? null : element.getContainingFile();
        if (containingFile != null && containingFile.getFileType() instanceof JavaScriptFileTypeWithVersion fileType
            && containingFile.getLanguage() == TypeScriptLanguage.INSTANCE) {
            return fileType.getLanguageVersion(element.getModule(), containingFile.getVirtualFile());
        }
        return BaseTypeScriptLanguageVersion.getLatest();
    }

    @RequiredReadAction
    @Override
    public LanguageVersion getLanguageVersion(Language language, @Nullable Project project, @Nullable VirtualFile virtualFile) {
        return BaseTypeScriptLanguageVersion.getLatest();
    }

    @Override
    public Language getLanguage() {
        return TypeScriptLanguage.INSTANCE;
    }
}
