var NodeIterator = {
    detach: function () {}, //Object
    expandEntityReferences: false,
    filter: {}, //NodeFilter
    nextNode: function () {}, //Node
    previousNode: function () {}, //Node
    root: {}, //Node
    whatToShow: 0,
};

var NodeFilter = {
    NodeFilter: function (param) {}, //Number
};

var TreeWalker = {
    firstChild: function () {}, //Node
    lastChild: function () {}, //Node
    nextNode: function () {}, //Node
    nextSibling: function () {}, //Node
    parentNode: function () {}, //Node
    previousNode: function () {}, //Node
    previousSibling: function () {}, //Node
};

var DocumentTraversal = {
    createNodeIterator: function (root, whatToShow, filter, entityReferenceExpansion) {}, //NodeIterator
    createTreeWalker: function (root, whatToShow, filter, entityReferenceExpansion) {}, //TreeWalker
};

var Range = {
    cloneContents: function () {}, //DocumentFragment
    cloneRange: function () {}, //Range
    collapse: function (toStart) {}, //Object
    collapsed: false,
    commonAncestorContainer: {}, //Node
    compareBoundaryPoints: function (how,sourceRange) {}, //short
    createContextualFragment: function (tagString) {}, //Gecko,Object
    deleteContents: function () {}, //Object
    detach: function () {}, //Object
    endContainer: {}, //Node
    endOffset: 0,
    extractContents: function () {}, //DocumentFragment
    insertNode: function (newNode) {}, //Object
    selectNode: function (refNode) {}, //Object
    selectNodeContents: function (refNode) {}, //Object
    setEnd: function (refNode,offset) {}, //Object
    setEndAfter: function (refNode) {}, //Object
    setEndBefore: function (refNode) {}, //Object
    setStart: function (refNode, offset) {}, //Object
    setStartAfter: function (refNode) {}, //Object
    setStartBefore: function (refNode) {}, //Object
    startContainer: {}, //Node
    startOffset: 0,
    surroundContents: function (newParent) {}, //Object
    toString: function () {}, //String
};

var DocumentRange = {
    createRange: function () {}, //Range
};

var RangeException = {
    code: 0,
};
