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

import com.intellij.psi.PsiManager;
import com.intellij.openapi.project.Project;
import gnu.trove.TIntObjectHashMap;

import java.io.DataInputStream;

/**
 * @by Maxim.Mossienko
*/
class DeserializationContext {
  final DataInputStream inputStream;
  final TIntObjectHashMap<String> myNames;
  final TIntObjectHashMap<JSNamespace> myNameSpaces = new TIntObjectHashMap<JSNamespace>();
  final TIntObjectHashMap<JSPackage> myPackages = new TIntObjectHashMap<JSPackage>();
  final JSTypeEvaluateManager typeEvaluateManager;
  final BrowserSupportManager browserSupportManager;
  final PsiManager manager;
  final JavaScriptIndex index;

  DeserializationContext(DataInputStream _inputStream, PsiManager _manager, TIntObjectHashMap<String> names) {
    inputStream = _inputStream;
    manager = _manager;
    myNames =  names;
    
    final Project project = manager.getProject();
    typeEvaluateManager = JSTypeEvaluateManager.getInstance(project);
    browserSupportManager = BrowserSupportManager.getInstance(project);
    index = JavaScriptIndex.getInstance(project);
  }
}
