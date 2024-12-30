var Event = {
    currentTarget: 0, //Gecko,EventTarget
    bubbles: 0, //Gecko,Boolean
    cancelable: 0, //Gecko,Boolean
    eventPhase: 0, //Gecko,Number
    initEvent: function (eventTypeArg, canBubbleArg, cancelableArg) {}, //Gecko,Object
    initEventNS: function (namespaceURIArg, eventTypeArg, canBubbleArg, cancelableArg) {}, //Gecko,Object
    isCustom: function () {}, //Gecko,Boolean
    isDefaultPrevented: function () {}, //Gecko,Boolean
    namespaceURI: 0, //Gecko,String
    preventDefault: function () {}, //Gecko,Object
    stopImmediatePropagation: function () {}, //Gecko,Object
    stopPropagation: function () {}, //Gecko,Object
    target: 0, //Gecko,EventTarget
    timeStamp: 0, //Gecko,Number
    type: 0, //Gecko,String
};

var EventTarget = {
    addEventListener: function (type,listener,useCapture) {}, //Gecko,Object
    addEventListenerNS: function (namespaceURI, type, listener, useCapture, evtGroup) {}, //Gecko,Object
    dispatchEvent: function (evt) {}, //Gecko,Boolean
    hasEventListenerNS: function (namespaceURI, type) {}, //Gecko,Boolean
    removeEventListener: function (type,listener,useCapture) {}, //Gecko,Object
    removeEventListenerNS: function (namespaceURI, type, listener, useCapture) {}, //Gecko,Object
    willTriggerNS: function (namespaceURI, type) {}, //Gecko,Boolean
};

var EventListener = {
    handleEvent: function (evt) {}, //Gecko,Object
};

var EventException = {
    code: 0, //Gecko,Number
};

var DocumentEvent = {
    canDispatch: function (namespaceURI, type) {}, //Gecko,String
    createEvent: function (eventType) {}, //Gecko,Event
};

var CustomEvent = {
    isImmediatePropagationStopped: function () {}, //Gecko,Boolean
    isPropagationStopped: function () {}, //Gecko,Boolean
    setDispatchState: function (target, phase) {}, //Gecko,Object
};
CustomEvent.prototype = new Event();

var UIEvent = {
    detail: 0, //Gecko,Number
    initUIEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) {}, //Gecko,Object
    initUIEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) {}, //Gecko,Object
    view: 0, //Gecko,AbstractView
};
UIEvent.prototype = new Event();

var TextEvent = {
    data: 0, //Gecko,String
    initTextEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) {}, //Gecko,Object
    initTextEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) {}, //Gecko,Object
};
TextEvent.prototype = new UIEvent();

var MouseEvent = {
    altKey: 0, //Gecko,Boolean
    button: 0, //Gecko,Number
    clientX: 0, //Gecko,Number
    clientY: 0, //Gecko,Number
    ctrlKey: 0, //Gecko,Boolean
    getModifierState: function (keyIdentifierArg) {}, //Gecko,Boolean
    initMouseEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) {}, //Gecko,Object
    initMouseEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) {}, //Gecko,Object
    metaKey: 0, //Gecko,Boolean
    screenX: 0, //Gecko,Number
    screenY: 0, //Gecko,Number
    shiftKey: 0, //Gecko,Boolean
    relatedTarget: 0, //Gecko,EventTarget
};
MouseEvent.prototype = new UIEvent();

var KeyboardEvent = {
    altKey: 0, //Gecko,Boolean
    ctrlKey: 0, //Gecko,Boolean
    getModifierState: function (keyIdentifierArg) {}, //Gecko,Boolean
    initKeyboardEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) {}, //Gecko,Object
    initKeyboardEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) {}, //Gecko,Object
    keyIdentifier: 0, //Gecko,String
    keyLocation: 0, //Gecko,Number
    metaKey: 0, //Gecko,Boolean
    shiftKey: 0, //Gecko,Boolean
};
KeyboardEvent.prototype = new UIEvent();

var MutationEvent = {
    attrChange: 0, //Gecko,Number
    attrName: 0, //Gecko,String
    initMutationEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) {}, //Gecko,Object
    initMutationEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) {}, //Gecko,Object
    newValue: 0, //Gecko,String
    prevValue: 0, //Gecko,String
    relatedNode: 0, //Gecko,Node
};
MutationEvent.prototype = new UIEvent();

var MutationNameEvent = {
    initMutationNameEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) {}, //Gecko,Object
    initMutationNameEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) {}, //Gecko,Object
    prevNamespaceURI: 0, //Gecko,String
    prevNodeName: 0, //Gecko,String
};
MutationNameEvent.prototype = new MutationEvent();
