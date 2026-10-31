package consulo.typescript.language.impl;

import consulo.annotation.access.RequiredReadAction;
import consulo.language.Language;
import consulo.language.ast.ASTNode;
import consulo.language.ast.ILazyParseableElementType;
import consulo.language.parser.ParserDefinition;
import consulo.language.parser.PsiBuilder;
import consulo.language.parser.PsiBuilderFactory;
import consulo.language.parser.PsiParser;
import consulo.language.psi.PsiElement;
import consulo.language.version.LanguageVersion;
import consulo.language.version.LanguageVersionUtil;
import consulo.project.Project;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.TypeScriptEmbeddedContentImpl;

/**
 * TypeScript embedded into another file: the content of {@code <script lang="ts">} (HTML, Vue). Parsed by the
 * TypeScript parser like a file; its PSI is {@link consulo.typescript.language.impl.psi.TypeScriptEmbeddedContentImpl}.
 *
 * @since 2026-10-31
 */
public interface TypeScriptEmbeddedElementTypes {
    ILazyParseableElementType EMBEDDED_CONTENT = new ILazyParseableElementType("TYPESCRIPT_EMBEDDED_CONTENT", TypeScriptLanguage.INSTANCE) {
        @Override
        @RequiredReadAction
        protected ASTNode doParseContents(ASTNode chameleon, PsiElement psi) {
            Language language = getLanguage();
            LanguageVersion languageVersion = LanguageVersionUtil.findDefaultVersion(language);
            Project project = psi.getProject();
            PsiBuilder builder = PsiBuilderFactory.getInstance()
                .createBuilder(project, chameleon, null, language, languageVersion, chameleon.getChars());
            PsiParser parser = ParserDefinition.forLanguage(language).createParser(languageVersion);
            return parser.parse(this, builder, languageVersion).getFirstChildNode();
        }

        @Override
        public ASTNode createNode(CharSequence text) {
            return new TypeScriptEmbeddedContentImpl(this, text);
        }
    };
}
