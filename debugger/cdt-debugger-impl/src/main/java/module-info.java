/**
 * @author VISTALL
 * @since 2026-02-15
 */
module consulo.javascript.cdt.debugger.impl {
    requires consulo.javascript.debugger.impl;

    requires com.google.gson;

    requires cdt.java.client;

    exports consulo.javascript.debugger.cdt;
}