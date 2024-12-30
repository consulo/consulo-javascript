var DOMImplementationRegistry = {
    getDOMImplementation: function (features) {}, //Object
    getDOMImplementationList: function (features) {}, //Object
};

var DOMException = {
    code: 0, //Number
};

var DOMStringList = {
    contains: function (str) {}, //Boolean
    item: function (index) {}, //String
    length: 0, //Number
};

var NameList = {
    contains: function (str) {}, //Boolean
    containsNS: function (namespaceURI,name) {}, //Boolean
    getName: function (index) {}, //String
    getNamespaceURI: function (index) {}, //String
    length: 0, //Number
};

var DOMImplementationList = {
    item: function (index) {}, //DOMImplementation
    length: 0, //Number
};

var DOMImplementationSource = {
    getDOMImplementation: function (features) {}, //DOMImplementation
    getDOMImplementationList: function (features) {}, //DOMImplementationList
};

var DOMImplementation = {
    createDocument: function (namespaceURI,qualifiedName,doctype) {}, //Document
    createDocumentType: function (qualifiedName,publicId,systemId) {}, //DocumentType
    getFeature: function (feature,version) {}, //Object
    hasFeature: function (feature,version) {}, //Boolean
};

var DocumentFragment = {};
DocumentFragment.prototype = new Node();

var Document = {
    adoptNode: function (source) {}, //Node
    createAttribute: function (name) {}, //Attr
    createAttributeNS: function (namespaceURI, qualifiedName) {}, //Attr
    createCDATASection: function (data) {}, //CDATASection
    createComment: function (data) {}, //Comment
    createDocumentFragment: function () {}, //DocumentFragment
    createElement: function (tagName) {}, //Element
    createElementNS: function (namespaceURI,qualifiedName) {}, //Element
    createProcessingInstruction: function (target,data) {}, //ProcessingInstruction
    createTextNode: function (data) {}, //Text
    createEntityReference: function (name) {}, //EntityReference
    doctype: 0, //DocumentType
    documentElement: 0, //Element
    documentURI: 0, //String
    domConfig: 0, //DOMConfiguration
    getElementById: function (elementId) {}, //Element
    getElementsByTagName: function (tagname) {}, //NodeList
    getElementsByTagNameNS: function (namespaceURI,localName) {}, //NodeList
    implementation: 0, //DOMImplementation
    importNode: function (importedNode,deep) {}, //Node
    inputEncoding: 0, //String
    parseError: 0, //IE,IXMLDOMParseError
    load: function (filename) {}, //IE,Object
    loadXML: function (xmlString) {}, //IE,Object
    normalizeDocument: function () {}, //Object
    renameNode: function (n,namespaceURI,qualifiedName) {}, //Node
    save: function (filename) {}, //IE,Object
    setProperty: function (name,value) {}, //IE,Object
    selectSingleNode: function (xpathExpression) {}, //IE,Object
    selectNodes: function (xpathExpression) {}, //IE,Object
    strictErrorChecking: 0, //Boolean
    xmlEncoding: 0, //String
    xmlStandalone: 0, //Boolean
    xmlVersion: 0, //String
};
Document.prototype = new Node();

var IXMLDOMParseError = {
    errorCode: 0, //IE,Number
    reason: 0, //IE,String
};

var Node = {
    nodeName: 0, //String
    nodeValue: 0, //String
    nodeType: 0, //Number
    parentNode: 0, //Node
    childNodes: 0, //NodeList
    firstChild: 0, //Node
    lastChild: 0, //Node
    previousSibling: 0, //Node
    nextSibling: 0, //Node
    attributes: 0, //NamedNodeMap
    ownerDocument: 0, //Document
    namespaceURI: 0, //String
    prefix: 0, //String
    localName: 0, //String
    baseURI: 0, //String
    textContent: 0, //String
    insertBefore: function (newChild,refChild) {}, //Node
    replaceChild: function (newChild,refChild) {}, //Node
    removeChild: function (oldChild) {}, //Node
    appendChild: function (newChild) {}, //Node
    hasChildNodes: function () {}, //Boolean
    cloneNode: function (deep) {}, //Node
    normalize: function () {}, //Object
    isSupported: function (feature,version) {}, //Boolean
    hasAttributes: function () {}, //Boolean
    compareDocumentPosition: function (other) {}, //Node
    isSameNode: function (other) {}, //Boolean
    lookupPrefix: function (namespaceURI) {}, //String
    isDefaultNamespace: function (namespaceURI) {}, //String
    lookupNamespaceURI: function (prefix) {}, //String
    isEqualNode: function (arg) {}, //Boolean
    getFeature: function (feature,version) {}, //Object
    setUserData: function (key,data,handler) {}, //Object
    getUserData: function (key) {}, //Object
};

var NodeList = {
    length: 0, //Number
    item: function (index) {}, //Node
};

var NamedNodeMap = {
    length: 0, //Number
    getNamedItem: function (name) {}, //Node
    setNamedItem: function (arg) {}, //Node
    removeNamedItem: function (name) {}, //Node
    item: function (index) {}, //Node
    getNamedItemNS: function (namespaceURI, localName) {}, //Node
    setNamedItemNS: function (arg) {}, //Node
    removeNamedItemNS: function (namespaceURI, localName) {}, //Node
};

var CharacterData = {
    data: 0, //String
    length: 0, //Number
    substringData: function (offset,count) {}, //String
    appendData: function (arg) {}, //Object
    insertData: function (offset,arg) {}, //Object
    deleteData: function (offset,count) {}, //Object
    replaceData: function (offset,count,arg) {}, //Object
};
CharacterData.prototype = new Node();

var Attr = {
    name: 0, //String
    specified: 0, //Boolean
    value: 0, //String
    ownerElement: 0, //Element
    schemaTypeInfo: 0, //TypeInfo
    isId: 0, //Boolean
};
Attr.prototype = new Node();

var Element = {
    getAttribute: function (name) {}, //String
    getAttributeNode: function (name) {}, //Attr
    getAttributeNodeNS: function (namespaceURI,localName) {}, //Object
    getAttributeNS: function (namespaceURI, localName) {}, //String
    getElementsByTagName: function (name) {}, //NodeList
    getElementsByTagNameNS: function (namespaceURI, localName) {}, //NodeList
    hasAttribute: function (name) {}, //Boolean
    hasAttributeNS: function (namespaceURI, localName) {}, //Boolean
    removeAttribute: function (name) {}, //Object
    removeAttributeNode: function (oldAttr) {}, //Attr
    removeAttributeNS: function (namespaceURI,localName) {}, //Object
    schemaTypeInfo: 0, //TypeInfo
    setAttribute: function (name,value) {}, //Object
    setAttributeNode: function (newAttr) {}, //Attr
    setAttributeNodeNS: function (newAttr) {}, //Object
    setAttributeNS: function (namespaceURI, qualifiedName, value) {}, //Object
    setIdAttribute: function (name, isId) {}, //Object
    setIdAttributeNode: function (idAttr, isId) {}, //Object
    setIdAttributeNS: function (namespaceURI, localName, isId) {}, //Object
    tagName: 0, //String
};
Element.prototype = new Node();

var Text = {
    isElementContentWhitespace: 0, //Boolean
    replaceWholeText: function (content) {}, //Text
    splitText: function (offset) {}, //Text
    wholeText: 0, //String
};
Text.prototype = new CharacterData();

var Comment = {};
Comment.prototype = new CharacterData();

var TypeInfo = {
    isDerivedFrom: function (typeNamespaceArg,typeNameArg,derivationMethod) {}, //Boolean
    typeName: 0, //String
    typeNamespace: 0, //String
};

var UserDataHandler = {
    handle: function (param1, param2, param3, param4, param5) {}, //Object
};

var DOMError = {
    location: 0, //DOMLocator
    message: 0, //String
    relatedData: 0, //Object
    relatedException: 0, //Object
    severity: 0, //Number
    type: 0, //String
};

var DOMErrorHandler = {
    handler: function (error) {}, //Boolean
};

var DOMLocator = {
    byteOffset: 0, //Number
    columnNumber: 0, //Number
    lineNumber: 0, //Number
    relatedNode: 0, //Node
    uri: 0, //String
    utf16Offset: 0, //Number
};

var DOMConfiguration = {
    canSetParameter: function (name, value) {}, //Boolean
    getParameter: function (name) {}, //Object
    parameterNames: 0, //DOMStringList
    setParameter: function (name, value) {}, //Object
};

var CDATASection = {};
CDATASection.prototype = new Text();

var DocumentType = {
    entities: 0, //NamedNodeMap
    internalSubset: 0, //String
    name: 0, //String
    notations: 0, //NamedNodeMap
    publicId: 0, //String
    systemId: 0, //String
};
DocumentType.prototype = new Node();

var Notation = {
    publicId: 0, //String
    systemId: 0, //String
};
Notation.prototype = new Node();

var Entity = {
    inputEncoding: 0, //String
    notationName: 0, //String
    publicId: 0, //String
    systemId: 0, //String
    xmlEncoding: 0, //String
    xmlVersion: 0, //String
};
Entity.prototype = new Node();

var EntityReference = {};
EntityReference.prototype = new Node();

var ProcessingInstruction = {
    data: 0, //String
    target: 0, //String
};
ProcessingInstruction.prototype = new Node();
