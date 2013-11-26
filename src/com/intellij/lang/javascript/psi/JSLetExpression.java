package com.intellij.lang.javascript.psi;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 14, 2005
 * Time: 6:37:59 PM
 * To change this template use File | Settings | File Templates.
 */
public interface JSLetExpression extends JSExpression {
  JSExpression getExpression();
}