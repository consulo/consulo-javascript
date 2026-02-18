/**
 * @author VISTALL
 * @since 2026-02-15
 */
module consulo.javascript.browser.debugger.impl {
    requires consulo.javascript.debugger.impl;

    requires com.google.gson;

    requires cdt.java.client;

    requires consulo.javascript.cdt.debugger.impl;

    opens consulo.javascript.debugger.browser to consulo.util.xml.serializer;
}