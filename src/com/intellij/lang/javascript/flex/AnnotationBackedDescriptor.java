package com.intellij.lang.javascript.flex;

import com.intellij.xml.XmlAttributeDescriptor;
import com.intellij.xml.XmlElementDescriptor;
import com.intellij.xml.XmlElementDescriptorWithCDataContent;

/**
 * @author Maxim.Mossienko
 */
public interface AnnotationBackedDescriptor extends XmlAttributeDescriptor, XmlElementDescriptor, XmlElementDescriptorWithCDataContent
{
	String getType();

	String getArrayType();
}
