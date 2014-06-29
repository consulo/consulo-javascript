DOMImplementationRegistry.getDOMImplementation = function(features) {};//Object
DOMImplementationRegistry.getDOMImplementationList = function(features) {};//Object
DOMImplementationRegistry.prototype = new Object();

DOMException.code = 0;//Number
DOMException = {};
DOMException.prototype = new Object();

DOMStringList.length = 0;//Number
DOMStringList = {};
DOMStringList.item = function(index) {};//String
DOMStringList.contains = function(str) {};//Boolean
DOMStringList.prototype = new Object();

NameList.length = 0;//Number
NameList = {};
NameList.getName = function(index) {};//String
NameList.getNamespaceURI = function(index) {};//String
NameList.contains = function(str) {};//Boolean
NameList.containsNS = function(namespaceURI,name) {};//Boolean
NameList.prototype = new Object();

DOMImplementationList.length = 0;//Number
DOMImplementationList = {};
DOMImplementationList.item = function(index) {};//DOMImplementation
DOMImplementationList.prototype = new Object();

DOMImplementationSource.getDOMImplementation = function(features) {};//DOMImplementation
DOMImplementationSource.getDOMImplementationList = function(features) {};//DOMImplementationList
DOMImplementationSource.prototype = new Object();

DOMImplementation.hasFeature = function(feature,version) {};//Boolean
DOMImplementation.createDocumentType = function(qualifiedName,publicId,systemId) {};//DocumentType
DOMImplementation.createDocument = function(namespaceURI,qualifiedName,doctype) {};//Document
DOMImplementation.getFeature = function(feature,version) {};//Object
DOMImplementation.prototype = new Object();

DocumentFragment.prototype = new Node();

Document.doctype = 0;//DocumentType
Document.implementation = 0;//DOMImplementation
Document.documentElement = 0;//Element
Document.inputEncoding = 0;//String
Document.xmlEncoding = 0;//String
Document.xmlStandalone = 0;//Boolean
Document.xmlVersion = 0;//String
Document.strictErrorChecking = 0;//Boolean
Document.documentURI = 0;//String
Document.domConfig = 0;//DOMConfiguration
Document.parseError = 0;//IE,IXMLDOMParseError
Document = {};
Document.createElement = function(tagName) {};//Element
Document.createDocumentFragment = function() {};//DocumentFragment
Document.createTextNode = function(data) {};//Text
Document.createComment = function(data) {};//Comment
Document.createCDATASection = function(data) {};//CDATASection
Document.createProcessingInstruction = function(target,data) {};//ProcessingInstruction
Document.createAttribute = function(name) {};//Attr
Document.createEntityReference = function(name) {};//EntityReference
Document.getElementsByTagName = function(tagname) {};//NodeList
Document.importNode = function(importedNode,deep) {};//Node
Document.createElementNS = function(namespaceURI,qualifiedName) {};//Element
Document.createAttributeNS = function(namespaceURI,qualifiedName) {};//Attr
Document.getElementsByTagNameNS = function(namespaceURI,localName) {};//NodeList
Document.getElementById = function(elementId) {};//Element
Document.adoptNode = function(source) {};//Node
Document.normalizeDocument = function() {};//Object
Document.renameNode = function(n,namespaceURI,qualifiedName) {};//Node
Document.load = function(filename) {};//IE,Object
Document.loadXML = function(xmlString) {};//IE,Object
Document.save = function(filename) {};//IE,Object
Document.setProperty = function(name,value) {};//IE,Object
Document.selectSingleNode = function(xpathExpression) {};//IE,Object
Document.selectNodes = function(xpathExpression) {};//IE,Object
Document.prototype = new Node();

IXMLDOMParseError.reason = 0;//IE,String
IXMLDOMParseError.errorCode = 0;//IE,Number
IXMLDOMParseError = {};
IXMLDOMParseError.prototype = new Object();

Node.nodeName = 0;//String
Node.nodeValue = 0;//String
Node.nodeType = 0;//Number
Node.parentNode = 0;//Node
Node.childNodes = 0;//NodeList
Node.firstChild = 0;//Node
Node.lastChild = 0;//Node
Node.previousSibling = 0;//Node
Node.nextSibling = 0;//Node
Node.attributes = 0;//NamedNodeMap
Node.ownerDocument = 0;//Document
Node.namespaceURI = 0;//String
Node.prefix = 0;//String
Node.localName = 0;//String
Node.baseURI = 0;//String
Node.textContent = 0;//String
Node = {};
Node.insertBefore = function(newChild,refChild) {};//Node
Node.replaceChild = function(newChild,refChild) {};//Node
Node.removeChild = function(oldChild) {};//Node
Node.appendChild = function(newChild) {};//Node
Node.hasChildNodes = function() {};//Boolean
Node.cloneNode = function(deep) {};//Node
Node.normalize = function() {};//Object
Node.isSupported = function(feature,version) {};//Boolean
Node.hasAttributes = function() {};//Boolean
Node.compareDocumentPosition = function(other) {};//Node
Node.isSameNode = function(other) {};//Boolean
Node.lookupPrefix = function(namespaceURI) {};//String
Node.isDefaultNamespace = function(namespaceURI) {};//String
Node.lookupNamespaceURI = function(prefix) {};//String
Node.isEqualNode = function(arg) {};//Boolean
Node.getFeature = function(feature,version) {};//Object
Node.setUserData = function(key,data,handler) {};//Object
Node.getUserData = function(key) {};//Object
Node.prototype = new Object();

NodeList.length = 0;//Number
NodeList = {};
NodeList.item = function(index) {};//Node
NodeList.prototype = new Object();

NamedNodeMap.length = 0;//Number
NamedNodeMap = {};
NamedNodeMap.getNamedItem = function(name) {};//Node
NamedNodeMap.setNamedItem = function(arg) {};//Node
NamedNodeMap.removeNamedItem = function(name) {};//Node
NamedNodeMap.item = function(index) {};//Node
NamedNodeMap.getNamedItemNS = function(namespaceURI,localName) {};//Node
NamedNodeMap.setNamedItemNS = function(arg) {};//Node
NamedNodeMap.removeNamedItemNS = function(namespaceURI,localName) {};//Node
NamedNodeMap.prototype = new Object();

CharacterData.data = 0;//String
CharacterData.length = 0;//Number
CharacterData = {};
CharacterData.substringData = function(offset,count) {};//String
CharacterData.appendData = function(arg) {};//Object
CharacterData.insertData = function(offset,arg) {};//Object
CharacterData.deleteData = function(offset,count) {};//Object
CharacterData.replaceData = function(offset,count,arg) {};//Object
CharacterData.prototype = new Node();

Attr.name = 0;//String
Attr.specified = 0;//Boolean
Attr.value = 0;//String
Attr.ownerElement = 0;//Element
Attr.schemaTypeInfo = 0;//TypeInfo
Attr.isId = 0;//Boolean
Attr = {};
Attr.prototype = new Node();

Element.tagName = 0;//String
Element.schemaTypeInfo = 0;//TypeInfo
Element = {};
Element.getAttribute = function(name) {};//String
Element.setAttribute = function(name,value) {};//Object
Element.removeAttribute = function(name) {};//Object
Element.getAttributeNode = function(name) {};//Attr
Element.setAttributeNode = function(newAttr) {};//Attr
Element.removeAttributeNode = function(oldAttr) {};//Attr
Element.getElementsByTagName = function(name) {};//NodeList
Element.getAttributeNS = function(namespaceURI,localName) {};//String
Element.setAttributeNS = function(namespaceURI,qualifiedName,value) {};//Object
Element.removeAttributeNS = function(namespaceURI,localName) {};//Object
Element.getAttributeNodeNS = function(namespaceURI,localName) {};//Object
Element.setAttributeNodeNS = function(newAttr) {};//Object
Element.getElementsByTagNameNS = function(namespaceURI,localName) {};//NodeList
Element.hasAttribute = function(name) {};//Boolean
Element.hasAttributeNS = function(namespaceURI,localName) {};//Boolean
Element.setIdAttribute = function(name,isId) {};//Object
Element.setIdAttributeNS = function(namespaceURI,localName,isId) {};//Object
Element.setIdAttributeNode = function(idAttr,isId) {};//Object
Element.prototype = new Node();

Text.isElementContentWhitespace = 0;//Boolean
Text.wholeText = 0;//String
Text = {};
Text.splitText = function(offset) {};//Text
Text.replaceWholeText = function(content) {};//Text
Text.prototype = new CharacterData();

Comment.prototype = new CharacterData();

TypeInfo.typeName = 0;//String
TypeInfo.typeNamespace = 0;//String
TypeInfo = {};
TypeInfo.isDerivedFrom = function(typeNamespaceArg,typeNameArg,derivationMethod) {};//Boolean
TypeInfo.prototype = new Object();

UserDataHandler.handle = function(param1,param2,param3,param4,param5) {};//Object
UserDataHandler.prototype = new Object();

DOMError.severity = 0;//Number
DOMError.message = 0;//String
DOMError.type = 0;//String
DOMError.relatedException = 0;//Object
DOMError.relatedData = 0;//Object
DOMError.location = 0;//DOMLocator
DOMError = {};
DOMError.prototype = new Object();

DOMErrorHandler.handler = function(error) {};//Boolean
DOMErrorHandler.prototype = new Object();

DOMLocator.lineNumber = 0;//Number
DOMLocator.columnNumber = 0;//Number
DOMLocator.byteOffset = 0;//Number
DOMLocator.utf16Offset = 0;//Number
DOMLocator.relatedNode = 0;//Node
DOMLocator.uri = 0;//String
DOMLocator = {};
DOMLocator.prototype = new Object();

DOMConfiguration.parameterNames = 0;//DOMStringList
DOMConfiguration = {};
DOMConfiguration.setParameter = function(name,value) {};//Object
DOMConfiguration.getParameter = function(name) {};//Object
DOMConfiguration.canSetParameter = function(name,value) {};//Boolean
DOMConfiguration.prototype = new Object();

CDATASection.prototype = new Text();

DocumentType.name = 0;//String
DocumentType.entities = 0;//NamedNodeMap
DocumentType.notations = 0;//NamedNodeMap
DocumentType.publicId = 0;//String
DocumentType.systemId = 0;//String
DocumentType.internalSubset = 0;//String
DocumentType = {};
DocumentType.prototype = new Node();

Notation.publicId = 0;//String
Notation.systemId = 0;//String
Notation = {};
Notation.prototype = new Node();

Entity.publicId = 0;//String
Entity.systemId = 0;//String
Entity.notationName = 0;//String
Entity.inputEncoding = 0;//String
Entity.xmlEncoding = 0;//String
Entity.xmlVersion = 0;//String
Entity = {};
Entity.prototype = new Node();

EntityReference.prototype = new Node();

ProcessingInstruction.target = 0;//String
ProcessingInstruction.data = 0;//String
ProcessingInstruction = {};
ProcessingInstruction.prototype = new Node();
