/**
 * @author VISTALL
 * @since 29-Aug-22
 */
module consulo.javascript.debugger.impl
{
	// TODO temp dep
	requires java.desktop;

	requires transitive consulo.javascript.base.api;

	requires transitive consulo.javascript.base.impl;

	requires transitive consulo.execution.debug.api;

	requires com.intellij.xml;

	exports consulo.javascript.debugger;
}