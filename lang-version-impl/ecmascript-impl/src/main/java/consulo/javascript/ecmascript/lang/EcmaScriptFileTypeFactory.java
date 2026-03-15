package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.virtualFileSystem.fileType.FileTypeConsumer;
import consulo.virtualFileSystem.fileType.FileTypeFactory;


/**
 * @author VISTALL
 * @since 05.03.2015
 */
@ExtensionImpl
public class EcmaScriptFileTypeFactory extends FileTypeFactory {
    @Override
    public void createFileTypes(FileTypeConsumer consumer) {
        consumer.consume(EcmaScriptFileType.INSTANCE, "es;js2");
    }
}
