/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.language.psi.PsiElement;
import consulo.xml.psi.xml.XmlFile;
import org.intellij.idea.lang.javascript.psiutil.ArrayStack;

import java.util.Iterator;
import java.util.NoSuchElementException;

public abstract class JSElementIterator implements Iterator<PsiElement> {
    private boolean reverse;
    private final int minTextOffset;
    private final int maxTextOffset;
    private ArrayStack<PsiElement> elementStack;
    private PsiElement next;
    private boolean retrieveNext;

    public JSElementIterator(PsiElement element) {
        this(element, false, 0, Integer.MAX_VALUE);
    }

    public JSElementIterator(PsiElement element, boolean reverse) {
        this(element, reverse, 0, Integer.MAX_VALUE);
    }

    public JSElementIterator(PsiElement element, boolean reverse, int minTextOffset, int maxTextOffset) {
        this.reverse = reverse;
        this.minTextOffset = minTextOffset;
        this.maxTextOffset = maxTextOffset;
        this.retrieveNext = true;
        this.elementStack = new ArrayStack<PsiElement>();

        if (element instanceof XmlFile) {
            for (JSElement embeddedElement : JSFunctionVisitor.getEmbeddedJSElements((XmlFile)element)) {
                if (reverse) {
                    this.elementStack.add(0, embeddedElement);
                }
                else {
                    this.elementStack.add(embeddedElement);
                }
            }
        }
        else if (element instanceof JSElement) {
            this.elementStack.push(element);
        }
        else {
            throw new ClassCastException(element.getClass().getName());
        }
    }

    /**
     * Returns <tt>true</tt> if this element should be part of the iteration,
     * <tt>false</tt> otherwise.
     *
     * @param element this element
     * @return <tt>true</tt> if this element should be part of the iteration,
     * <tt>false</tt> otherwise.
     */
    protected abstract boolean visitElement(PsiElement element);

    private void findNext() {
        PsiElement element = null;
        boolean pass = false;

        if (!this.elementStack.empty()) {
            do {
                element = this.elementStack.pop();

                int elementTextOffset = element.getTextOffset();
                int elementTextEndOffset = elementTextOffset + element.getTextLength();

                if (elementTextEndOffset >= this.minTextOffset &&
                    elementTextOffset <= this.maxTextOffset) {
                    pass = this.visitElement(element);
                    this.pushChildren(
                        element.getChildren(),
                        this.reverse ? elementTextOffset : elementTextEndOffset
                    );
                }
            }
            while (!pass && !this.elementStack.empty());
        }

        this.next = pass ? element : null;
        this.retrieveNext = false;
    }

    private void pushChildren(PsiElement[] children, int elementTextOffset) {
        int childTextOffset = elementTextOffset;

        if (this.reverse) {
            int index = 0;

            while (index < children.length && childTextOffset <= this.maxTextOffset) {
                PsiElement child = children[index];

                childTextOffset = child.getTextOffset();
                if (childTextOffset <= this.maxTextOffset) {
                    this.elementStack.push(child);
                }
                index++;
            }
        }
        else {
            int index = children.length - 1;

            while (index >= 0 && childTextOffset >= this.minTextOffset) {
                PsiElement child = children[index];

                childTextOffset = child.getTextOffset() + child.getTextLength();
                if (childTextOffset >= this.minTextOffset) {
                    this.elementStack.push(child);
                }
                index--;
            }
        }
    }

    @Override
    public boolean hasNext() {
        if (this.retrieveNext) {
            this.findNext();
        }

        return (this.next != null);
    }

    @Override
    public PsiElement next() {
        if (this.retrieveNext) {
            this.findNext();  // hasNext() has not been called
        }
        if (this.next == null) {
            throw new NoSuchElementException();
        }

        this.retrieveNext = true;
        return this.next;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
