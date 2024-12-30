var NodeIterator = {
    detach: function () { return {}; },
    expandEntityReferences: false,
    filter: {}, //NodeFilter
    nextNode: function () { return {}; }, //Node
    previousNode: function () { return {}; }, //Node
    root: {}, //Node
    whatToShow: 0,
};

var NodeFilter = {
    NodeFilter: function (param) { return 0; },
};

var TreeWalker = {
    firstChild: function () { return {}; }, //Node
    lastChild: function () { return {}; }, //Node
    nextNode: function () { return {}; }, //Node
    nextSibling: function () { return {}; }, //Node
    parentNode: function () { return {}; }, //Node
    previousNode: function () { return {}; }, //Node
    previousSibling: function () { return {}; }, //Node
};

var DocumentTraversal = {
    createNodeIterator: function (root, whatToShow, filter, entityReferenceExpansion) { return {}; }, //NodeIterator
    createTreeWalker: function (root, whatToShow, filter, entityReferenceExpansion) { return {}; }, //TreeWalker
};

var Range = {
    cloneContents: function () { return {}; }, //DocumentFragment
    cloneRange: function () { return {}; }, //Range
    collapse: function (toStart) { return {}; },
    collapsed: false,
    commonAncestorContainer: {}, //Node
    compareBoundaryPoints: function (how, sourceRange) { return 0; },
    createContextualFragment: function (tagString) { return {}; }, //Gecko
    deleteContents: function () { return {}; },
    detach: function () { return {}; },
    endContainer: {}, //Node
    endOffset: 0,
    extractContents: function () { return {}; }, //DocumentFragment
    insertNode: function (newNode) { return {}; },
    selectNode: function (refNode) { return {}; },
    selectNodeContents: function (refNode) { return {}; },
    setEnd: function (refNode,offset) { return {}; },
    setEndAfter: function (refNode) { return {}; },
    setEndBefore: function (refNode) { return {}; },
    setStart: function (refNode, offset) { return {}; },
    setStartAfter: function (refNode) { return {}; },
    setStartBefore: function (refNode) { return {}; },
    startContainer: {}, //Node
    startOffset: 0,
    surroundContents: function (newParent) { return {}; },
    toString: function () { return ""; },
};

var DocumentRange = {
    createRange: function () { return {}; }, //Range
};

var RangeException = {
    code: 0,
};
