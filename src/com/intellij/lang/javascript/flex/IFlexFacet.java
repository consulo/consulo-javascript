package com.intellij.lang.javascript.flex;

/**
 * @author yole
 */
public interface IFlexFacet {
  /** getConfiguration() causes problems with JDK 1.5 */
  FlexFacetConfiguration getFlexConfiguration();
}
