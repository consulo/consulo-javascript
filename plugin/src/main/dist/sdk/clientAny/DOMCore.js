var DOMImplementationRegistry = {
    getDOMImplementation: function (features) { return {}; },
    getDOMImplementationList: function (features) { return {}; },
};

var DOMException = {
    code: 0,
};

var DOMStringList = {
    contains: function (str) { return false; },
    item: function (index) { return ""; },
    length: 0,
};

var NameList = {
    contains: function (str) { return false; },
    containsNS: function (namespaceURI, name) { return false; },
    getName: function (index) { return ""; },
    getNamespaceURI: function (index) { return ""; },
    length: 0,
};

var DOMImplementationList = {
    item: function (index) {}, //DOMImplementation
    length: 0,
};

var DOMImplementationSource = {
    getDOMImplementation: function (features) { return {}; }, //DOMImplementation
    getDOMImplementationList: function (features) { return {}; }, //DOMImplementationList
};

var DOMImplementation = {
    createDocument: function (namespaceURI, qualifiedName, doctype) { return {}; }, //Document
    createDocumentType: function (qualifiedName, publicId, systemId) { return {}; }, //DocumentType
    getFeature: function (feature, version) { return {}; },
    hasFeature: function (feature, version) { return false; },
};

var DocumentFragment = {};
DocumentFragment.prototype = new Node();

var Document = {
    adoptNode: function (source) { return {}; }, //Node
    createAttribute: function (name) { return {}; }, //Attr
    createAttributeNS: function (namespaceURI, qualifiedName) { return {}; }, //Attr
    createCDATASection: function (data) { return {}; }, //CDATASection
    createComment: function (data) { return {}; }, //Comment
    createDocumentFragment: function () { return {}; }, //DocumentFragment
    createElement: function (tagName) { return {}; }, //Element
    createElementNS: function (namespaceURI, qualifiedName) { return {}; }, //Element
    createProcessingInstruction: function (target, data) { return {}; }, //ProcessingInstruction
    createTextNode: function (data) { return {}; }, //Text
    createEntityReference: function (name) { return {}; }, //EntityReference
    doctype: {}, //DocumentType
    documentElement: {}, //Element
    documentURI: "",
    domConfig: {}, //DOMConfiguration
    getElementById: function (elementId) { return {}; }, //Element
    getElementsByTagName: function (tagname) { return {}; }, //NodeList
    getElementsByTagNameNS: function (namespaceURI, localName) { return {}; }, //NodeList
    implementation: {}, //DOMImplementation
    importNode: function (importedNode, deep) { return {}; }, //Node
    inputEncoding: "",
    parseError: {}, //IE,IXMLDOMParseError
    load: function (filename) { return {}; }, //IE
    loadXML: function (xmlString) { return {}; }, //IE
    normalizeDocument: function () { return {}; },
    renameNode: function (n, namespaceURI, qualifiedName) { return {}; }, //Node
    save: function (filename) { return {}; }, //IE
    setProperty: function (name, value) { return {}; }, //IE
    selectSingleNode: function (xpathExpression) { return {}; }, //IE
    selectNodes: function (xpathExpression) { return {}; }, //IE
    strictErrorChecking: false,
    xmlEncoding: "",
    xmlStandalone: false,
    xmlVersion: "",
};
Document.prototype = new Node();

var IXMLDOMParseError = {
    errorCode: 0, //IE
    reason: "", //IE
};

var Node = {
    appendChild: function (newChild) {}, //Node
    attributes: {}, //NamedNodeMap
    baseURI: "",
    childNodes: {}, //NodeList
    cloneNode: function (deep) {}, //Node
    compareDocumentPosition: function (other) {}, //Node
    firstChild: {}, //Node
    getFeature: function (feature, version) { return {}; },
    getUserData: function (key) { return {}; },
    hasAttributes: function () { return false; },
    hasChildNodes: function () { return false; },
    isDefaultNamespace: function (namespaceURI) { return ""; },
    isEqualNode: function (arg) { return false; },
    isSameNode: function (other) { return false; },
    isSupported: function (feature, version) { return false; },
    lookupNamespaceURI: function (prefix) { return ""; },
    lookupPrefix: function (namespaceURI) { return ""; },
    insertBefore: function (newChild, refChild) { return {}; }, //Node
    lastChild: {}, //Node
    localName: "",
    nodeName: "",
    nodeValue: "",
    nodeType: 0,
    namespaceURI: "",
    nextSibling: {}, //Node
    normalize: function () { return {}; },
    ownerDocument: {}, //Document
    parentNode: {}, //Node
    prefix: "",
    previousSibling: {}, //Node
    replaceChild: function (newChild, refChild) { return {}; }, //Node
    removeChild: function (oldChild) { return {}; }, //Node
    setUserData: function (key, data, handler) { return {}; },
    textContent: "",
};

var NodeList = {
    length: 0,
    item: function (index) {}, //Node
};

var NamedNodeMap = {
    length: 0,
    getNamedItem: function (name) { return {}; }, //Node
    setNamedItem: function (arg) { return {}; }, //Node
    removeNamedItem: function (name) { return {}; }, //Node
    item: function (index) { return {}; }, //Node
    getNamedItemNS: function (namespaceURI, localName) { return {}; }, //Node
    setNamedItemNS: function (arg) { return {}; }, //Node
    removeNamedItemNS: function (namespaceURI, localName) { return {}; }, //Node
};

var CharacterData = {
    data: "",
    length: 0,
    substringData: function (offset,count) { return ""; },
    appendData: function (arg) { return {}; },
    insertData: function (offset,arg) { return {}; },
    deleteData: function (offset,count) { return {}; },
    replaceData: function (offset,count,arg) { return {}; },
};
CharacterData.prototype = new Node();

var Attr = {
    name: "",
    specified: false,
    value: "",
    ownerElement: {}, //Element
    schemaTypeInfo: {}, //TypeInfo
    isId: false,
};
Attr.prototype = new Node();

var Element = {
    getAttribute: function (name) { return ""; },
    getAttributeNode: function (name) { return {}; }, //Attr
    getAttributeNodeNS: function (namespaceURI, localName) { return {}; },
    getAttributeNS: function (namespaceURI, localName) { return ""; },
    getElementsByTagName: function (name) { return {}; }, //NodeList
    getElementsByTagNameNS: function (namespaceURI, localName) { return {}; }, //NodeList
    hasAttribute: function (name) { return false; },
    hasAttributeNS: function (namespaceURI, localName) { return false; },
    removeAttribute: function (name) { return {}; },
    removeAttributeNode: function (oldAttr) {}, //Attr
    removeAttributeNS: function (namespaceURI, localName) { return {}; },
    schemaTypeInfo: {}, //TypeInfo
    setAttribute: function (name,value) { return {}; },
    setAttributeNode: function (newAttr) { return {}; }, //Attr
    setAttributeNodeNS: function (newAttr) { return {}; },
    setAttributeNS: function (namespaceURI, qualifiedName, value) { return {}; },
    setIdAttribute: function (name, isId) { return {}; },
    setIdAttributeNode: function (idAttr, isId) { return {}; },
    setIdAttributeNS: function (namespaceURI, localName, isId) { return {}; },
    tagName: "",
};
Element.prototype = new Node();

var Text = {
    isElementContentWhitespace: false,
    replaceWholeText: function (content) { return {}; }, //Text
    splitText: function (offset) { return {}; }, //Text
    wholeText: "",
};
Text.prototype = new CharacterData();

var Comment = {};
Comment.prototype = new CharacterData();

var TypeInfo = {
    isDerivedFrom: function (typeNamespaceArg, typeNameArg, derivationMethod) { return false; },
    typeName: "",
    typeNamespace: "",
};

var UserDataHandler = {
    handle: function (param1, param2, param3, param4, param5) { return {}; },
};

var DOMError = {
    location: {}, //DOMLocator
    message: "",
    relatedData: {}, //Object
    relatedException: {}, //Object
    severity: 0,
    type: "",
};

var DOMErrorHandler = {
    handler: function (error) { return false; },
};

var DOMLocator = {
    byteOffset: 0,
    columnNumber: 0,
    lineNumber: 0,
    relatedNode: 0, //Node
    uri: "",
    utf16Offset: 0,
};

var DOMConfiguration = {
    canSetParameter: function (name, value) { return false; },
    getParameter: function (name) { return {}; },
    parameterNames: 0, //DOMStringList
    setParameter: function (name, value) { return {}; },
};

var CDATASection = {};
CDATASection.prototype = new Text();

var DocumentType = {
    entities: {}, //NamedNodeMap
    internalSubset: "",
    name: "",
    notations: {}, //NamedNodeMap
    publicId: "",
    systemId: "",
};
DocumentType.prototype = new Node();

var Notation = {
    publicId: "",
    systemId: "",
};
Notation.prototype = new Node();

var Entity = {
    inputEncoding: "",
    notationName: "",
    publicId: "",
    systemId: "",
    xmlEncoding: "",
    xmlVersion: "",
};
Entity.prototype = new Node();

var EntityReference = {};
EntityReference.prototype = new Node();

var ProcessingInstruction = {
    data: "",
    target: "",
};
ProcessingInstruction.prototype = new Node();
