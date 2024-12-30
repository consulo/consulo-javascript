var XPathException = {
    code: 0, //Gecko
};

var XPathEvaluator = {
    createExpression: function (expression, resolver) {}, //Gecko,XPathExpression
    createNSResolver: function (nodeResolver) {}, //Gecko,XPathNSResolver
    evaluate: function (expression, contextNode, resolver, type, result) {}, //Gecko,Object
};

var XPathExpression = {
    evaluate: function (contextNode, type, result) {}, //Gecko,Object
};

var XPathNSResolver = {
    lookupNamespaceURI: function (prefix) {}, //Gecko,String
};

var XPathResult = {
    ANY_TYPE: 0, //Gecko
    ANY_UNORDERED_NODE_TYPE: 0, //Gecko
    BOOLEAN_TYPE: 0, //Gecko
    booleanValue: false, //Gecko
    FIRST_ORDERED_NODE_TYPE: 0, //Gecko
    invalidIteratorState: false, //Gecko
    iterateNext: function () {}, //Gecko,Node
    NUMBER_TYPE: 0, //Gecko
    numberValue: 0, //Gecko
    ORDERED_NODE_ITERATOR_TYPE: 0, //Gecko
    ORDERED_NODE_SNAPSHOT_TYPE: 0, //Gecko
    resultType: 0, //Gecko
    singleNodeValue: {}, //Gecko,Node
    snapshotItem: function (index) {}, //Gecko,Node
    snapshotLength: 0, //Gecko
    STRING_TYPE: 0, //Gecko
    stringValue: "", //Gecko
    UNORDERED_NODE_ITERATOR_TYPE: 0, //Gecko
    UNORDERED_NODE_SNAPSHOT_TYPE: 0, //Gecko
};

var XPathNamespace = {
    ownerElement: {}, //Gecko,Element
};
XPathNamespace.prototype = new Node();
