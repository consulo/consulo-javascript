var DOMImplementationRegistry = {
    getDOMImplementation: function (features) {}, //Object
    getDOMImplementationList: function (features) {}, //Object
};

var DOMException = {
    code: 0,
};

var DOMStringList = {
    contains: function (str) {}, //Boolean
    item: function (index) {}, //String
    length: 0,
};

var NameList = {
    contains: function (str) {}, //Boolean
    containsNS: function (namespaceURI,name) {}, //Boolean
    getName: function (index) {}, //String
    getNamespaceURI: function (index) {}, //String
    length: 0,
};

var DOMImplementationList = {
    item: function (index) {}, //DOMImplementation
    length: 0,
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
    doctype: {}, //DocumentType
    documentElement: {}, //Element
    documentURI: "",
    domConfig: {}, //DOMConfiguration
    getElementById: function (elementId) {}, //Element
    getElementsByTagName: function (tagname) {}, //NodeList
    getElementsByTagNameNS: function (namespaceURI,localName) {}, //NodeList
    implementation: {}, //DOMImplementation
    importNode: function (importedNode,deep) {}, //Node
    inputEncoding: "",
    parseError: {}, //IE,IXMLDOMParseError
    load: function (filename) {}, //IE,Object
    loadXML: function (xmlString) {}, //IE,Object
    normalizeDocument: function () {}, //Object
    renameNode: function (n,namespaceURI,qualifiedName) {}, //Node
    save: function (filename) {}, //IE,Object
    setProperty: function (name,value) {}, //IE,Object
    selectSingleNode: function (xpathExpression) {}, //IE,Object
    selectNodes: function (xpathExpression) {}, //IE,Object
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
    getFeature: function (feature,version) {}, //Object
    getUserData: function (key) {}, //Object
    hasAttributes: function () {}, //Boolean
    hasChildNodes: function () {}, //Boolean
    isDefaultNamespace: function (namespaceURI) {}, //String
    isEqualNode: function (arg) {}, //Boolean
    isSameNode: function (other) {}, //Boolean
    isSupported: function (feature,version) {}, //Boolean
    lookupNamespaceURI: function (prefix) {}, //String
    lookupPrefix: function (namespaceURI) {}, //String
    insertBefore: function (newChild,refChild) {}, //Node
    lastChild: {}, //Node
    localName: "",
    nodeName: "",
    nodeValue: "",
    nodeType: 0,
    namespaceURI: "",
    nextSibling: {}, //Node
    normalize: function () {}, //Object
    ownerDocument: {}, //Document
    parentNode: {}, //Node
    prefix: "",
    previousSibling: {}, //Node
    replaceChild: function (newChild,refChild) {}, //Node
    removeChild: function (oldChild) {}, //Node
    setUserData: function (key,data,handler) {}, //Object
    textContent: "",
};

var NodeList = {
    length: 0,
    item: function (index) {}, //Node
};

var NamedNodeMap = {
    length: 0,
    getNamedItem: function (name) {}, //Node
    setNamedItem: function (arg) {}, //Node
    removeNamedItem: function (name) {}, //Node
    item: function (index) {}, //Node
    getNamedItemNS: function (namespaceURI, localName) {}, //Node
    setNamedItemNS: function (arg) {}, //Node
    removeNamedItemNS: function (namespaceURI, localName) {}, //Node
};

var CharacterData = {
    data: "",
    length: 0,
    substringData: function (offset,count) {}, //String
    appendData: function (arg) {}, //Object
    insertData: function (offset,arg) {}, //Object
    deleteData: function (offset,count) {}, //Object
    replaceData: function (offset,count,arg) {}, //Object
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
    schemaTypeInfo: {}, //TypeInfo
    setAttribute: function (name,value) {}, //Object
    setAttributeNode: function (newAttr) {}, //Attr
    setAttributeNodeNS: function (newAttr) {}, //Object
    setAttributeNS: function (namespaceURI, qualifiedName, value) {}, //Object
    setIdAttribute: function (name, isId) {}, //Object
    setIdAttributeNode: function (idAttr, isId) {}, //Object
    setIdAttributeNS: function (namespaceURI, localName, isId) {}, //Object
    tagName: "",
};
Element.prototype = new Node();

var Text = {
    isElementContentWhitespace: false,
    replaceWholeText: function (content) {}, //Text
    splitText: function (offset) {}, //Text
    wholeText: "",
};
Text.prototype = new CharacterData();

var Comment = {};
Comment.prototype = new CharacterData();

var TypeInfo = {
    isDerivedFrom: function (typeNamespaceArg, typeNameArg, derivationMethod) {}, //Boolean
    typeName: "",
    typeNamespace: "",
};

var UserDataHandler = {
    handle: function (param1, param2, param3, param4, param5) {}, //Object
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
    handler: function (error) {}, //Boolean
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
    canSetParameter: function (name, value) {}, //Boolean
    getParameter: function (name) {}, //Object
    parameterNames: 0, //DOMStringList
    setParameter: function (name, value) {}, //Object
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
