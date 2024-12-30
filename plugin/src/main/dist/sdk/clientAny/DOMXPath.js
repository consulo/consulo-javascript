var XPathException = {
    code: 0, //Gecko,Number
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
    ANY_TYPE: 0, //Gecko,Number
    ANY_UNORDERED_NODE_TYPE: 0, //Gecko,Number
    BOOLEAN_TYPE: 0, //Gecko,Number
    booleanValue: 0, //Gecko,Boolean
    FIRST_ORDERED_NODE_TYPE: 0, //Gecko,Number
    invalidIteratorState: 0, //Gecko,Boolean
    iterateNext: function () {}, //Gecko,Node
    NUMBER_TYPE: 0, //Gecko,Number
    numberValue: 0, //Gecko,Number
    ORDERED_NODE_ITERATOR_TYPE: 0, //Gecko,Number
    ORDERED_NODE_SNAPSHOT_TYPE: 0, //Gecko,Number
    resultType: 0, //Gecko,Number
    singleNodeValue: 0, //Gecko,Node
    snapshotItem: function (index) {}, //Gecko,Node
    snapshotLength: 0, //Gecko,Number
    STRING_TYPE: 0, //Gecko,Number
    stringValue: 0, //Gecko,String
    UNORDERED_NODE_ITERATOR_TYPE: 0, //Gecko,Number
    UNORDERED_NODE_SNAPSHOT_TYPE: 0, //Gecko,Number
};

var XPathNamespace = {
    ownerElement: 0, //Gecko,Element
};
XPathNamespace.prototype = new Node();
