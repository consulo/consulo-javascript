package com.intellij.lang.javascript.index;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.index.predefined.Marker;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.util.LocalTimeCounter;
import com.intellij.testFramework.LightVirtualFile;
import org.jdom.Document;
import org.jdom.Element;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * @by maxim.mossienko
 */
public class PredefinedJSIndexEntry extends JSIndexEntry {
  public PredefinedJSIndexEntry(final DeserializationContext context, @NotNull String fileName) throws IOException {
    super(context, new LightVirtualFile(fileName, generateText(fileName)));
    ((LightVirtualFile)myFile).setWritable(false);
  }

  public PredefinedJSIndexEntry(final @NotNull String fileName, Project project, boolean lazy) {
    super(new LightVirtualFile(fileName, generateText(fileName)), project, lazy, null);
    ((LightVirtualFile)myFile).setWritable(false);
  }

  private static CharSequence generateText(final String fileName) {
    return loadText(fileName, "xml".equals(fileName.substring(fileName.indexOf('.') + 1)));
  }

  protected PsiFile buildPsiFileFromFile() {
    String fileName = myFile.getName();
    final int dotIndex = fileName.indexOf('.');

    final String ext = fileName.substring(dotIndex + 1);
    final boolean isGeneratedCode = "xml".equals(ext);

    final String jsFileName =
      isGeneratedCode ? JavaScriptIndex.PREDEFINES_PREFIX +
                        fileName.substring(0, dotIndex) + JavaScriptIndex.PREDEFINED_SCRIPTS_FILE_EXTENSION : fileName;
    final PsiFile fileFromText = createReadOnlyFileFromText(((LightVirtualFile)myFile).getContent().toString(), myIndex.getProject(), jsFileName);
    fileFromText.putUserData(JavaScriptIndex.PREDEFINED_JS_FILE_KEY, "");
    return fileFromText;
  }

  public static @Nullable PsiFile createReadOnlyFileFromText(final @Nullable String text, @NotNull Project project, @NotNull final String jsFileName) {
    if (text == null) return null;

    PsiFile file = PsiFileFactory.getInstance(project)
        .createFileFromText(jsFileName, JavaScriptSupportLoader.JAVASCRIPT, text, LocalTimeCounter.currentTime(), false, false);
    file.putUserData(JavaScriptIndex.READONLY_JS_FILE_KEY, "");
    final VirtualFile vfile = file.getVirtualFile();
    if (vfile != null) ((LightVirtualFile)vfile).setWritable(false);

    return file;
  }

  protected static String loadText(String myFileName, final boolean generatedCode) {
    String s;

    try {
      final InputStream stream = Marker.class.getResourceAsStream(myFileName);
      assert stream != null : "Corrupted installation, missed file :" + myFileName;

      if (generatedCode) {
        s = translateFile(stream, myFileName);
      }
      else {
        s = FileUtil.loadTextAndClose(new InputStreamReader(stream));
      }
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }
    return s;
  }

  @NonNls private static final String NAME_ATTR_NAME = "name";
  @NonNls private static final String METHOD_TAG_NAME = "method";
  @NonNls private static final String PARAM_TAG_NAME = "param";
  @NonNls private static final String PROPERTY_TAG_NAME = "property";
  @NonNls private static final String EVENT_TAG_NAME = "event";
  @NonNls private static final String BROWSER_ATTR_NAME = "browser";
  @NonNls private static final String DEPRECATED_ATTR_NAME = "deprecated";

  @NonNls private static final String GLOBAL_CLASS_NAME = "Global";
  @NonNls private static final String OBJECT_CLASS_NAME = "Object";

  private static String translateFile(InputStream inputStream, final String fileName) {
    try {
      final Document document = JDOMUtil.loadDocument(inputStream);
      final List elements = document.getRootElement().getChildren("class");
      StringBuilder builder = new StringBuilder(8192);

      for (Object e : elements) {
        if (e instanceof Element) {
          if (builder.length() > 0) builder.append('\n');
          final Element element = (Element)e;
          String className = element.getAttributeValue(NAME_ATTR_NAME);
          String extendsFromName = element.getAttributeValue("extends");
          if (extendsFromName == null && !GLOBAL_CLASS_NAME.equals(className) && !OBJECT_CLASS_NAME.equals(className)) {
            extendsFromName = OBJECT_CLASS_NAME;
          }

          translateOneClass(element, className, extendsFromName, builder);
        }
      }

      if (fileName.equals(JavaScriptIndex.DHTML_XML_FILE_NAME)) {
        try {
          Class cssPropertyTableClass = Class.forName("com.intellij.psi.css.impl.util.table.CssElementDescriptorFactory");
          final Method method = cssPropertyTableClass.getMethod("getPropertyNames");
          Set<String> propertyNames = (Set<String>)method.invoke(null);

          StringBuilder result = new StringBuilder();
          for (String propertyName : propertyNames) {
            result.setLength(0);
            result.ensureCapacity(propertyName.length());
            StringTokenizer tokenizer = new StringTokenizer(propertyName, "-");

            while (tokenizer.hasMoreTokens()) {
              String token = tokenizer.nextToken();
              if (result.length() != 0) token = StringUtil.capitalize(token);
              result.append(token);
            }

            builder.append("style.").append(result.toString()).append(" = 0;\n");
          }
        }
        catch (Exception e) {
        }
      }
      return builder.toString();
    }
    catch (Exception e) {
      JavaScriptIndex.LOG.error(e);
    }
    return null;
  }

  private static void translateOneClass(final Element element,
                                        final String className,
                                        final String extendsClassName,
                                        final StringBuilder builder) {
    //builder.append("public class ").append(className);
    //if (extendsClassName != null) builder.append(" extends ").append(extendsClassName);
    //builder.append(" {\n");

    String targetBrowser = element.getAttributeValue(BROWSER_ATTR_NAME);
    String selectionExpr = !className.equals(GLOBAL_CLASS_NAME) ? className + '.' : "";

    List children = element.getChildren(PROPERTY_TAG_NAME);
    processNodes(children, builder, selectionExpr, targetBrowser);

    children = element.getChildren(METHOD_TAG_NAME);
    processNodes(children, builder, selectionExpr, targetBrowser);

    children = element.getChildren(EVENT_TAG_NAME);
    processNodes(children, builder, selectionExpr, targetBrowser);

    if (extendsClassName != null) {
      builder.append(className).append(".prototype = new ").append(extendsClassName).append("();\n");
    }
    //builder.append("}\n");
  }

  private static void processNodes(final List children, final StringBuilder builder, final String selectionExpr, String browserSpecific) {
    boolean headerStarted = false;
    boolean processingProperties = false;
    boolean seenConstructor = false;

    for (Object e2 : children) {
      if (e2 instanceof Element) {
        final Element subelement = ((Element)e2);
        final String name = subelement.getAttributeValue(NAME_ATTR_NAME);
        final boolean method = METHOD_TAG_NAME.equals(subelement.getName());
        final boolean property = PROPERTY_TAG_NAME.equals(subelement.getName());
        final String type;
        final String typeValue;

        final boolean deprecated = "true".equals(subelement.getAttributeValue(DEPRECATED_ATTR_NAME));

        String elementBrowserSpecific = subelement.getAttributeValue(BROWSER_ATTR_NAME);
        if (elementBrowserSpecific == null) elementBrowserSpecific = browserSpecific;

        if (method) {
          final List grandchildren = subelement.getChildren(PARAM_TAG_NAME);
          String paramString = "";

          for (Object p : grandchildren) {
            final Element param = ((Element)p);

            //if (param.getAttributeValue("mandatory") == null) continue;
            final String paramName = param.getAttributeValue(NAME_ATTR_NAME);
            if (paramString.length() > 0) paramString += ",";
            paramString += paramName;
          }

          type = "function(" + paramString + ") {}";
          typeValue = subelement.getAttributeValue("returnType");
        }
        else if (property) {
          processingProperties = true;

          if ("constructor".equals(name)) {
            seenConstructor = true;
            builder.append(selectionExpr.substring(0, selectionExpr.length() - 1)).append(" = ").append("function() {};\n");
          }

          type = "0";
          typeValue = subelement.getAttributeValue("type");
        }
        else { // event
          if (!headerStarted) {
            headerStarted = true;
            builder.append("var ").append(selectionExpr.substring(0, selectionExpr.length() - 1)).append(" = {\n");
          }
          builder.append(name).append(": function () {},\n");
          continue;
        }

        builder.append(selectionExpr).append(name).append(" = ").append(type).append(";");
        JSIndexEntry.encodeBrowserSpecificsAndType(builder, elementBrowserSpecific, typeValue, deprecated);
        builder.append('\n');
      }
    }

    if (headerStarted) {
      builder.append("};\n");
    }

    if (processingProperties && !seenConstructor && selectionExpr.length() > 0) {
      // Create static var for noncostructable classes e.g. Math
      builder.append(selectionExpr.substring(0, selectionExpr.length() - 1)).append(" = {};\n");
    }
  }

  public long getTimeStamp() {
    return -1;
  }
}