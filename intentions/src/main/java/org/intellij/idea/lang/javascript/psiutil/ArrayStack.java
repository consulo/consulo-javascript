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
package org.intellij.idea.lang.javascript.psiutil;

import java.util.ArrayList;
import java.util.EmptyStackException;

/**
 * The <tt>ArrayStack</tt> class represents a last-in-first-out (LIFO)
 * stack of objects. It extends class <tt>ArrayList</tt> with five
 * operations that allow an array list to be treated as a stack. The
 * usual  <tt>push</tt> and <tt>pop</tt> operations are provided, as
 * well as a method to <tt>peek</tt> at the top item on the stack, a
 * method to test  for whether the stack is <tt>empty</tt>, and a
 * method to <tt>search</tt>  the stack for an item and discover how
 * far it is from the top.
 * <p>
 * When a stack is first created, it contains no items.
 * <p>
 * This stack is <b>not</b> thread-safe. If you need a thread-safe stack,
 * please use the standard {@link java.util.Stack java.util.Stack} class
 * instead.
 */
public class ArrayStack<E> extends ArrayList<E> implements Cloneable {

    /**
     * Creates an empty <tt>ArrayStack</tt>.
     */
    public ArrayStack() {
    }

    /**
     * Pushes an item onto the top of this stack. This has exactly
     * the same effect as:
     * <blockquote><pre>
     * add(item)</pre></blockquote>
     *
     * @param   item   the item to be pushed onto this stack.
     * @return  the <tt>item</tt> argument.
     * @see     ArrayList#add
     */
    public E push(E item) {
        this.add(item);
        return item;
    }

    /**
     * Removes the object at the top of this stack and returns that
     * object as the value of this function.
     *
     * @return  The object at the top of this stack (the last item
     *          of the <tt>ArrayList</tt> object).
     * @throws  EmptyStackException  if this stack is empty.
     */
    public E pop() {
        int  len = this.size();
        E    obj = this.peek();

        this.remove(len - 1);
        return obj;
    }

    /**
     * Looks at the object at the top of this stack without removing it
     * from the stack.
     *
     * @return  the object at the top of this stack (the last item
     *          of the <tt>ArrayList</tt> object).
     * @throws  EmptyStackException  if this stack is empty.
     */
    public E peek() {
        int  len = this.size();

        if (len == 0) {
            throw new EmptyStackException();
        }
        return this.get(len - 1);
    }

    /**
     * Tests if this stack is empty.
     *
     * @return  <tt>true</tt> if and only if this stack contains
     *          no items; <tt>false</tt> otherwise.
     */
    public boolean empty() {
        return (this.size() == 0);
    }

    /**
     * Returns the 1-based position where an object is on this stack.
     * If the object <tt>o</tt> occurs as an item in this stack, this
     * method returns the distance from the top of the stack of the
     * occurrence nearest the top of the stack; the topmost item on the
     * stack is considered to be at distance <tt>1</tt>. The <tt>equals</tt>
     * method is used to compare <tt>o</tt> to the
     * items in this stack.
     *
     * @param   o   the desired object.
     * @return  the 1-based position from the top of the stack where
     *          the object is located; the return value <tt>-1</tt>
     *          indicates that the object is not on the stack.
     */
    public int search(E o) {
        int  index = this.lastIndexOf(o);

        if (index >= 0) {
            return this.size() - index;
        }

        return -1;
    }

    /**
     * Returns a shallow copy of this <tt>ArrayList</tt> instance.  (The
     * elements themselves are not copied.)
     *
     * @return  a clone of this <tt>ArrayList</tt> instance.
     */
    @Override
	public Object clone() {
        return (ArrayStack<E>) super.clone();
    }

    /** use java.util.Stack#serialVersionUID from JDK 1.0.2 for interoperability */
    private static final long serialVersionUID = 1224463164541339165L;
}
