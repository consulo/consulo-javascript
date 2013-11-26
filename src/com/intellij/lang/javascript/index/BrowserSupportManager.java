/*
 * Copyright 2000-2006 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.index;

import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.project.Project;
import gnu.trove.THashSet;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Apr 26, 2006
 * Time: 5:14:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class BrowserSupportManager implements ProjectComponent {
  private Set<JSNamedElement> myIESpecificSymbols = new THashSet<JSNamedElement>(50);
  private Set<JSNamedElement> myGeckoSpecificSymbols = new THashSet<JSNamedElement>(50);
  private Set<JSNamedElement> myOperaSpecificSymbols = new THashSet<JSNamedElement>(50);
  //private Set<JSNamedElement> myW3SpecificSymbols = new THashSet<JSNamedElement>(50);

  public static BrowserSupportManager getInstance(Project project) {
    return project.getComponent(BrowserSupportManager.class);
  }

  public void projectOpened() {}
  public void projectClosed() {}

  @NonNls
  @NotNull
  public String getComponentName() {
    return "JS.BrowserSupportManager";
  }

  public void initComponent() {}

  public void disposeComponent() {}

  public void addIESpecificSymbol(JSNamedElement element) {
    myIESpecificSymbols.add(element);
  }

  public void addGeckoSpecificSymbol(JSNamedElement element) {
    myGeckoSpecificSymbols.add(element);
  }

  public void addOperaSpecificSymbol(JSNamedElement element) {
    myOperaSpecificSymbols.add(element);
  }
  
  public boolean isIESpecificSymbol(JSNamedElement element) {
    return myIESpecificSymbols.contains(element);
  }

  public boolean isGeckoSpecificSymbol(JSNamedElement element) {
    return myGeckoSpecificSymbols.contains(element);
  }

  public boolean isOperaSpecificSymbol(JSNamedElement element) {
    return myOperaSpecificSymbols.contains(element);
  }

  public void clear() {
    myGeckoSpecificSymbols.clear();
    myIESpecificSymbols.clear();
    myOperaSpecificSymbols.clear();
  }
}
