package com.intellij.lang.javascript.impl.validation;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.annotation.Annotator;
import consulo.language.editor.annotation.AnnotatorFactory;

import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2022-08-29
 */
@ExtensionImpl
public class JSAnnotatorFactory implements AnnotatorFactory {
    @Nullable
    @Override
    public Annotator createAnnotator() {
        return new JSAnnotatingVisitor();
    }

    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
