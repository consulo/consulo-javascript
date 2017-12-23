Event.type = 0;//Gecko,String
Event.target = 0;//Gecko,EventTarget
Event.currentTarget = 0;//Gecko,EventTarget
Event.eventPhase = 0;//Gecko,Number
Event.bubbles = 0;//Gecko,Boolean
Event.cancelable = 0;//Gecko,Boolean
Event.timeStamp = 0;//Gecko,Number
Event.namespaceURI = 0;//Gecko,String
Event = {};
Event.stopPropagation = function() {};//Gecko,Object
Event.preventDefault = function() {};//Gecko,Object
Event.initEvent = function(eventTypeArg,canBubbleArg,cancelableArg) {};//Gecko,Object
Event.isCustom = function() {};//Gecko,Boolean
Event.stopImmediatePropagation = function() {};//Gecko,Object
Event.isDefaultPrevented = function() {};//Gecko,Boolean
Event.initEventNS = function(namespaceURIArg,eventTypeArg,canBubbleArg,cancelableArg) {};//Gecko,Object
Event.prototype = new Object();

EventTarget.addEventListener = function(type,listener,useCapture) {};//Gecko,Object
EventTarget.removeEventListener = function(type,listener,useCapture) {};//Gecko,Object
EventTarget.dispatchEvent = function(evt) {};//Gecko,Boolean
EventTarget.addEventListenerNS = function(namespaceURI,type,listener,useCapture,evtGroup) {};//Gecko,Object
EventTarget.removeEventListenerNS = function(namespaceURI,type,listener,useCapture) {};//Gecko,Object
EventTarget.willTriggerNS = function(namespaceURI,type) {};//Gecko,Boolean
EventTarget.hasEventListenerNS = function(namespaceURI,type) {};//Gecko,Boolean
EventTarget.prototype = new Object();

EventListener.handleEvent = function(evt) {};//Gecko,Object
EventListener.prototype = new Object();

EventException.code = 0;//Gecko,Number
EventException = {};
EventException.prototype = new Object();

DocumentEvent.createEvent = function(eventType) {};//Gecko,Event
DocumentEvent.canDispatch = function(namespaceURI,type) {};//Gecko,String
DocumentEvent.prototype = new Object();

CustomEvent.setDispatchState = function(target,phase) {};//Gecko,Object
CustomEvent.isPropagationStopped = function() {};//Gecko,Boolean
CustomEvent.isImmediatePropagationStopped = function() {};//Gecko,Boolean
CustomEvent.prototype = new Event();

UIEvent.view = 0;//Gecko,AbstractView
UIEvent.detail = 0;//Gecko,Number
UIEvent = {};
UIEvent.initUIEvent = function(typeArg,canBubbleArg,cancelableArg,viewArg,detailArg) {};//Gecko,Object
UIEvent.initUIEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,viewArg,detailArg) {};//Gecko,Object
UIEvent.prototype = new Event();

TextEvent.data = 0;//Gecko,String
TextEvent = {};
TextEvent.initTextEvent = function(typeArg,canBubbleArg,cancelableArg,viewArg,dataArg) {};//Gecko,Object
TextEvent.initTextEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,viewArg,dataArg) {};//Gecko,Object
TextEvent.prototype = new UIEvent();

MouseEvent.screenX = 0;//Gecko,Number
MouseEvent.screenY = 0;//Gecko,Number
MouseEvent.clientX = 0;//Gecko,Number
MouseEvent.clientY = 0;//Gecko,Number
MouseEvent.ctrlKey = 0;//Gecko,Boolean
MouseEvent.shiftKey = 0;//Gecko,Boolean
MouseEvent.altKey = 0;//Gecko,Boolean
MouseEvent.metaKey = 0;//Gecko,Boolean
MouseEvent.button = 0;//Gecko,Number
MouseEvent.relatedTarget = 0;//Gecko,EventTarget
MouseEvent = {};
MouseEvent.getModifierState = function(keyIdentifierArg) {};//Gecko,Boolean
MouseEvent.initMouseEvent = function(typeArg,canBubbleArg,cancelableArg,viewArg,detailArg,screenXArg,screenYArg,clientXArg,clientYArg,ctrlKeyArg,altKeyArg,shiftKeyArg,metaKeyArg,buttonArg,relatedTargetArg) {};//Gecko,Object
MouseEvent.initMouseEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,viewArg,detailArg,screenXArg,screenYArg,clientXArg,clientYArg,ctrlKeyArg,altKeyArg,shiftKeyArg,metaKeyArg,buttonArg,relatedTargetArg) {};//Gecko,Object
MouseEvent.prototype = new UIEvent();

KeyboardEvent.keyIdentifier = 0;//Gecko,String
KeyboardEvent.keyLocation = 0;//Gecko,Number
KeyboardEvent.ctrlKey = 0;//Gecko,Boolean
KeyboardEvent.shiftKey = 0;//Gecko,Boolean
KeyboardEvent.altKey = 0;//Gecko,Boolean
KeyboardEvent.metaKey = 0;//Gecko,Boolean
KeyboardEvent = {};
KeyboardEvent.getModifierState = function(keyIdentifierArg) {};//Gecko,Boolean
KeyboardEvent.initKeyboardEvent = function(typeArg,canBubbleArg,cancelableArg,viewArg,keyIdentifierArg,keyLocationArg,modifiersList) {};//Gecko,Object
KeyboardEvent.initKeyboardEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,viewArg,keyIdentifierArg,keyLocationArg,modifiersList) {};//Gecko,Object
KeyboardEvent.prototype = new UIEvent();

MutationEvent.relatedNode = 0;//Gecko,Node
MutationEvent.prevValue = 0;//Gecko,String
MutationEvent.newValue = 0;//Gecko,String
MutationEvent.attrName = 0;//Gecko,String
MutationEvent.attrChange = 0;//Gecko,Number
MutationEvent = {};
MutationEvent.initMutationEvent = function(typeArg,canBubbleArg,cancelableArg,relatedNodeArg,prevValueArg,newValueArg,attrNameArg,attrChangeArg) {};//Gecko,Object
MutationEvent.initMutationEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,relatedNodeArg,prevValueArg,newValueArg,attrNameArg,attrChangeArg) {};//Gecko,Object
MutationEvent.prototype = new UIEvent();

MutationNameEvent.prevNamespaceURI = 0;//Gecko,String
MutationNameEvent.prevNodeName = 0;//Gecko,String
MutationNameEvent = {};
MutationNameEvent.initMutationNameEvent = function(typeArg,canBubbleArg,cancelableArg,relatedNodeArg,prevNamespaceURIArg,prevNodeNameArg) {};//Gecko,Object
MutationNameEvent.initMutationNameEventNS = function(namespaceURI,typeArg,canBubbleArg,cancelableArg,relatedNodeArg,prevNamespaceURIArg,prevNodeNameArg) {};//Gecko,Object
MutationNameEvent.prototype = new MutationEvent();
