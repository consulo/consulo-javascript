NodeIterator.root = 0;//Node
NodeIterator.whatToShow = 0;//Number
NodeIterator.filter = 0;//NodeFilter
NodeIterator.expandEntityReferences = 0;//Boolean
NodeIterator = {};
NodeIterator.nextNode = function() {};//Node
NodeIterator.previousNode = function() {};//Node
NodeIterator.detach = function() {};//Object
NodeIterator.prototype = new Object();

NodeFilter.NodeFilter = function(param) {};//Number
NodeFilter.prototype = new Object();

TreeWalker.parentNode = function() {};//Node
TreeWalker.firstChild = function() {};//Node
TreeWalker.lastChild = function() {};//Node
TreeWalker.previousSibling = function() {};//Node
TreeWalker.nextSibling = function() {};//Node
TreeWalker.previousNode = function() {};//Node
TreeWalker.nextNode = function() {};//Node
TreeWalker.prototype = new Object();

DocumentTraversal.createNodeIterator = function(root,whatToShow,filter,entityReferenceExpansion) {};//NodeIterator
DocumentTraversal.createTreeWalker = function(root,whatToShow,filter,entityReferenceExpansion) {};//TreeWalker
DocumentTraversal.prototype = new Object();

Range.startContainer = 0;//Node
Range.startOffset = 0;//long
Range.endContainer = 0;//Node
Range.endOffset = 0;//long
Range.collapsed = 0;//Boolean
Range.commonAncestorContainer = 0;//Node
Range = {};
Range.setStart = function(refNode,offset) {};//Object
Range.setEnd = function(refNode,offset) {};//Object
Range.setStartBefore = function(refNode) {};//Object
Range.setStartAfter = function(refNode) {};//Object
Range.setEndBefore = function(refNode) {};//Object
Range.setEndAfter = function(refNode) {};//Object
Range.collapse = function(toStart) {};//Object
Range.selectNode = function(refNode) {};//Object
Range.selectNodeContents = function(refNode) {};//Object
Range.compareBoundaryPoints = function(how,sourceRange) {};//short
Range.deleteContents = function() {};//Object
Range.extractContents = function() {};//DocumentFragment
Range.cloneContents = function() {};//DocumentFragment
Range.insertNode = function(newNode) {};//Object
Range.surroundContents = function(newParent) {};//Object
Range.cloneRange = function() {};//Range
Range.toString = function() {};//String
Range.detach = function() {};//Object
Range.createContextualFragment = function(tagString) {};//Gecko,Object
Range.prototype = new Object();

DocumentRange.createRange = function() {};//Range
DocumentRange.prototype = new Object();

RangeException.code = 0;//Number
RangeException = {};
RangeException.prototype = new Object();
