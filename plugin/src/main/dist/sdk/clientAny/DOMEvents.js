var Event = {
    currentTarget: {}, //Gecko,EventTarget
    bubbles: false, //Gecko
    cancelable: false, //Gecko
    eventPhase: 0, //Gecko
    initEvent: function (eventTypeArg, canBubbleArg, cancelableArg) { return {}; }, //Gecko
    initEventNS: function (namespaceURIArg, eventTypeArg, canBubbleArg, cancelableArg) { return {}; }, //Gecko
    isCustom: function () { return false; }, //Gecko
    isDefaultPrevented: function () { return false; }, //Gecko
    namespaceURI: "", //Gecko
    preventDefault: function () { return {}; }, //Gecko
    stopImmediatePropagation: function () { return {}; }, //Gecko
    stopPropagation: function () { return {}; }, //Gecko
    target: {}, //Gecko,EventTarget
    timeStamp: 0, //Gecko
    type: "", //Gecko
};

var EventTarget = {
    addEventListener: function (type,listener,useCapture) { return {}; }, //Gecko
    addEventListenerNS: function (namespaceURI, type, listener, useCapture, evtGroup) { return {}; }, //Gecko
    dispatchEvent: function (evt) { return false; }, //Gecko
    hasEventListenerNS: function (namespaceURI, type) { return false; }, //Gecko
    removeEventListener: function (type,listener,useCapture) { return {}; }, //Gecko
    removeEventListenerNS: function (namespaceURI, type, listener, useCapture) { return {}; }, //Gecko
    willTriggerNS: function (namespaceURI, type) { return false; }, //Gecko
};

var EventListener = {
    handleEvent: function (evt) { return {}; }, //Gecko
};

var EventException = {
    code: 0, //Gecko
};

var DocumentEvent = {
    canDispatch: function (namespaceURI, type) { return ""; }, //Gecko
    createEvent: function (eventType) { return {}; }, //Gecko,Event
};

var CustomEvent = {
    isImmediatePropagationStopped: function () { return false; }, //Gecko
    isPropagationStopped: function () { return false; }, //Gecko
    setDispatchState: function (target, phase) { return {}; }, //Gecko
};
CustomEvent.prototype = new Event();

var UIEvent = {
    detail: 0, //Gecko
    initUIEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) { return {}; }, //Gecko
    initUIEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg) { return {}; }, //Gecko
    view: {}, //Gecko,AbstractView
};
UIEvent.prototype = new Event();

var TextEvent = {
    data: "", //Gecko
    initTextEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) { return {}; }, //Gecko
    initTextEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, dataArg) { return {}; }, //Gecko
};
TextEvent.prototype = new UIEvent();

var MouseEvent = {
    altKey: false, //Gecko
    button: 0, //Gecko
    clientX: 0, //Gecko
    clientY: 0, //Gecko
    ctrlKey: false, //Gecko
    getModifierState: function (keyIdentifierArg) { return false; }, //Gecko
    initMouseEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) { return {}; }, //Gecko
    initMouseEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, detailArg, screenXArg, screenYArg, clientXArg, clientYArg, ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg, buttonArg, relatedTargetArg) { return {}; }, //Gecko
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
    getModifierState: function (keyIdentifierArg) { return false; }, //Gecko
    initKeyboardEvent: function (typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) { return {}; }, //Gecko
    initKeyboardEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, viewArg, keyIdentifierArg, keyLocationArg, modifiersList) { return {}; }, //Gecko
    keyIdentifier: "", //Gecko
    keyLocation: 0, //Gecko
    metaKey: false, //Gecko
    shiftKey: false, //Gecko
};
KeyboardEvent.prototype = new UIEvent();

var MutationEvent = {
    attrChange: 0, //Gecko
    attrName: "", //Gecko
    initMutationEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) { return {}; }, //Gecko
    initMutationEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg) { return {}; }, //Gecko
    newValue: "", //Gecko
    prevValue: "", //Gecko
    relatedNode: {}, //Gecko,Node
};
MutationEvent.prototype = new UIEvent();

var MutationNameEvent = {
    initMutationNameEvent: function (typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) { return {}; }, //Gecko
    initMutationNameEventNS: function (namespaceURI, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevNamespaceURIArg, prevNodeNameArg) { return {}; }, //Gecko
    prevNamespaceURI: "", //Gecko
    prevNodeName: "", //Gecko
};
MutationNameEvent.prototype = new MutationEvent();
