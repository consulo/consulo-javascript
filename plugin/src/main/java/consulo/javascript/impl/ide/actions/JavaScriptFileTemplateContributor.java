package consulo.javascript.impl.ide.actions;

import consulo.annotation.component.ExtensionImpl;
import consulo.fileTemplate.FileTemplateContributor;
import consulo.fileTemplate.FileTemplateRegistrator;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class JavaScriptFileTemplateContributor implements FileTemplateContributor {
    @Override
    public void register(FileTemplateRegistrator fileTemplateRegistrator) {
        fileTemplateRegistrator.registerInternalTemplate("JavaScriptFile");
    }
}
