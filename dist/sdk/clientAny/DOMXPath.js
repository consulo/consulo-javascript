XPathException.code = 0;//Gecko,Number
XPathException = {};
XPathException.prototype = new Object();

XPathEvaluator.createExpression = function(expression,resolver) {};//Gecko,XPathExpression
XPathEvaluator.createNSResolver = function(nodeResolver) {};//Gecko,XPathNSResolver
XPathEvaluator.evaluate = function(expression,contextNode,resolver,type,result) {};//Gecko,Object
XPathEvaluator.prototype = new Object();

XPathExpression.evaluate = function(contextNode,type,result) {};//Gecko,Object
XPathExpression.prototype = new Object();

XPathNSResolver.lookupNamespaceURI = function(prefix) {};//Gecko,String
XPathNSResolver.prototype = new Object();

XPathResult.resultType = 0;//Gecko,Number
XPathResult.numberValue = 0;//Gecko,Number
XPathResult.stringValue = 0;//Gecko,String
XPathResult.booleanValue = 0;//Gecko,Boolean
XPathResult.singleNodeValue = 0;//Gecko,Node
XPathResult.invalidIteratorState = 0;//Gecko,Boolean
XPathResult.snapshotLength = 0;//Gecko,Number
XPathResult.ORDERED_NODE_SNAPSHOT_TYPE = 0;//Gecko,Number
XPathResult.ORDERED_NODE_ITERATOR_TYPE = 0;//Gecko,Number
XPathResult.ANY_TYPE = 0;//Gecko,Number
XPathResult.BOOLEAN_TYPE = 0;//Gecko,Number
XPathResult.NUMBER_TYPE = 0;//Gecko,Number
XPathResult.STRING_TYPE = 0;//Gecko,Number
XPathResult.FIRST_ORDERED_NODE_TYPE = 0;//Gecko,Number
XPathResult.ANY_UNORDERED_NODE_TYPE = 0;//Gecko,Number
XPathResult.UNORDERED_NODE_ITERATOR_TYPE = 0;//Gecko,Number
XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE = 0;//Gecko,Number
XPathResult = {};
XPathResult.iterateNext = function() {};//Gecko,Node
XPathResult.snapshotItem = function(index) {};//Gecko,Node
XPathResult.prototype = new Object();

XPathNamespace.ownerElement = 0;//Gecko,Element
XPathNamespace = {};
XPathNamespace.prototype = new Node();
