/**
 * @author VISTALL
 * @since 29-Aug-22
 */
module consulo.javascript.json.javascript.impl
{
	// TODO remove in future
	requires java.desktop;

	requires consulo.javascript.base.impl;

	exports consulo.json;
	exports consulo.json.jom;
	exports consulo.json.jom.proxy;
	exports consulo.json.jom.proxy.impl;
	exports consulo.json.lang;
	exports consulo.json.lang.lexer;
	exports consulo.json.validation;
	exports consulo.json.validation.completion;
	exports consulo.json.validation.descriptionByAnotherPsiElement;
	exports consulo.json.validation.descriptor;
	exports consulo.json.validation.inspections;
	exports consulo.json.validation.psi.reference;
}