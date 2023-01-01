/**
 * @author VISTALL
 * @since 29-Aug-22
 */
module consulo.javascript.v8.debugger.impl
{
	requires consulo.javascript.debugger.impl;

	requires lib.org.chromium.sdk;

	exports consulo.javascript.run.debug.v8;
}