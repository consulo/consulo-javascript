var Event = {
    currentTarget: 0, //Gecko,EventTarget
    bubbles: false, //Gecko
    cancelable: false, //Gecko
    eventPhase: 0, //Gecko
    initEvent: function (eventTypeArg, canBubbleArg, cancelableArg) {}, //Gecko,Object
    initEventNS: function (namespaceURIArg, eventTypeArg, canBubbleArg, cancelableArg) {}, //Gecko,Object
    isCustom: function () {}, //Gecko,Boolean
    isDefaultPrevented: function () {}, //Gecko,Boolean
    namespaceURI: "", //Gecko
    preventDefault: function () {}, //Gecko,Object
    stopImmediatePropagation: function () {}, //Gecko,Object
    stopPropagation: function () {}, //Gecko,Object
    target: 0, //Gecko,EventTarget
    timeStamp: 0, //Gecko
    type: "", //Gecko
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
    code: 0, //Gecko
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
    detail: 0, //Gecko
    initUIEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) {}, //Gecko,Object
    initUIEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) {}, //Gecko,Object
    view: {}, //Gecko,AbstractView
};
UIEvent.prototype = new Event();

var TextEvent = {
    data: "", //Gecko
    initTextEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) {}, //Gecko,Object
    initTextEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) {}, //Gecko,Object
};
TextEvent.prototype = new UIEvent();

var MouseEvent = {
    altKey: false, //Gecko
    button: 0, //Gecko
    clientX: 0, //Gecko
    clientY: 0, //Gecko
    ctrlKey: false, //Gecko
    getModifierState: function (keyIdentifierArg) {}, //Gecko,Boolean
    initMouseEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) {}, //Gecko,Object
    initMouseEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) {}, //Gecko,Object
    metaKey: false, //Gecko
    screenX: 0, //Gecko
    screenY: 0, //Gecko
    shiftKey: false, //Gecko
    relatedTarget: {}, //Gecko,EventTarget
};
MouseEvent.prototype = new UIEvent();

var KeyboardEvent = {
    altKey: false, //Gecko
    ctrlKey: false, //Gecko
    getModifierState: function (keyIdentifierArg) {}, //Gecko,Boolean
    initKeyboardEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) {}, //Gecko,Object
    initKeyboardEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) {}, //Gecko,Object
    keyIdentifier: "", //Gecko
    keyLocation: 0, //Gecko
    metaKey: false, //Gecko
    shiftKey: false, //Gecko
};
KeyboardEvent.prototype = new UIEvent();

var MutationEvent = {
    attrChange: 0, //Gecko
    attrName: "", //Gecko
    initMutationEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) {}, //Gecko,Object
    initMutationEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) {}, //Gecko,Object
    newValue: "", //Gecko
    prevValue: "", //Gecko
    relatedNode: {}, //Gecko,Node
};
MutationEvent.prototype = new UIEvent();

var MutationNameEvent = {
    initMutationNameEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) {}, //Gecko,Object
    initMutationNameEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) {}, //Gecko,Object
    prevNamespaceURI: "", //Gecko
    prevNodeName: "", //Gecko
};
MutationNameEvent.prototype = new MutationEvent();
