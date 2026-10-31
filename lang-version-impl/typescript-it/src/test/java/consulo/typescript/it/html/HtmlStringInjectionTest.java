package consulo.typescript.it.html;

import consulo.application.ReadAction;
import consulo.html.language.HTMLLanguage;
import consulo.it.AllowLogError;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.Language;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.inject.advanced.Configuration;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.typescript.it.WebModules;
import org.jdom.Element;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * HTML is injected into the strings the DOM parses as markup, in JavaScript and TypeScript: the places of
 * {@code jsInjections.xml} (Settings | Language Injections).
 */
@ExtendWith(HeadlessProjectExtension.class)
// the XML plugin can't load completely headless (its default injections need jaxen)
@AllowLogError({
    "consulo.language.inject.advanced.Configuration",
    "consulo.language.impl.internal.psi.resolve.reference.ReferenceProvidersRegistryImpl",
    "consulo.xml.impl.internal.ExternalResource",
    "consulo.component.internal.NewExtensionPointImpl"
})
public class HtmlStringInjectionTest {
    /**
     * Web: the ECMAScript level of the module (template literals); the default injections.
     */
    @BeforeEach
    public void enableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
        // the default injections (jsInjections.xml) are imported by loadState, which a headless application without a
        // stored state doesn't call (until the platform with ApplicationInjectionConfiguration.afterLoad is installed)
        Configuration.getInstance().loadState(new Element(Configuration.COMPONENT_NAME));
    }

    @AfterEach
    public void disableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.disableWeb(fixture.getModule());
    }
    @Test
    public void testHtmlStrings(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            for (String text : List.of(
                "document.querySelector('#app').innerHTML = '<div class=\"a\"><<caret>b>x</b></div>';\n",
                "var el = document.body;\nel.outerHTML = \"<p><caret>a</p>\";\n",
                "var name = 'a';\ndocument.body.innerHTML = `<l<caret>i>${name}</li>`;\n",
                "document.body.insertAdjacentHTML('beforeend', '<sp<caret>an>a</span>');\n",
                "document.write('<h1><caret>a</h1>');\n",
                "new DOMParser().parseFromString('<p><caret>a</p>', 'text/html');\n"
            )) {
                assertEquals(HTMLLanguage.INSTANCE, injectedLanguage(fixture, extension, text), extension + ": " + text.trim());
            }
        }
    }

    @Test
    public void testOtherStrings(CodeInsightTestFixture fixture) throws Exception {
        for (String extension : List.of("js", "ts")) {
            for (String text : List.of(
                "document.title = '<b><caret>a</b>';\n",
                "document.body.insertAdjacentHTML('before<caret>end', '<b>a</b>');\n",
                "console.log('<b><caret>a</b>');\n"
            )) {
                assertNull(injectedLanguage(fixture, extension, text), extension + ": " + text.trim());
            }
        }
    }

    @Nullable
    private static Language injectedLanguage(CodeInsightTestFixture fixture, String extension, String text) throws Exception {
        PsiFile file = fixture.configureByText("inject" + Integer.toHexString(text.hashCode()) + "." + extension, text);
        int offset = text.indexOf("<caret>");
        return ReadAction.compute(() -> {
            PsiElement injected = InjectedLanguageManager.getInstance(fixture.getProject()).findInjectedElementAt(file, offset);
            return injected == null ? null : injected.getContainingFile().getLanguage();
        });
    }
}
