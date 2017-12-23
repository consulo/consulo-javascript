Object.__defineGetter__ = function(propertyName,func) {};//Gecko,Object
Object.__defineSetter__ = function(propertyName,func) {};//Gecko,Object
Object.__lookupGetter__ = function(propertyName) {};//Gecko,Function
Object.__lookupSetter__ = function(propertyName) {};//Gecko,Function
Object.__proto__ = function(propertyName) {};//Gecko,Function

Image = function() {};
Image.constructor = 0;//Object
Image.naturalWidth = 0;//Gecko,Object
Image.naturalHeight = 0;//Gecko,Object
Image.prototype = new HTMLImageElement();

Navigator.userAgent = 0;//String
Navigator.product = 0;//String
Navigator.appVersion = 0;//String
Navigator.appName = 0;//String
Navigator.appCodeName = 0;//String
Navigator.language = 0;//String
Navigator.systemLanguage = 0;//IE,String
Navigator.userLanguage = 0;//IE,String
Navigator.mimeTypes = 0;//Array
Navigator.platform = 0;//Array
Navigator.plugins = 0;//String
Navigator = {};
Navigator.taintEnabled = function() {};//Boolean
Navigator.javaEnabled = function() {};//Boolean
Navigator.preference = function(prefName,prefValue) {};//String
Navigator.prototype = new Object();

Screen.width = 0;//Number
Screen.height = 0;//Number
Screen.availHeight = 0;//Number
Screen.availWidth = 0;//Number
Screen.pixelDepth = 0;//Number
Screen.colorDepth = 0;//Number
Screen = {};
Screen.prototype = new Object();

location.href = 0;//String
location.hash = 0;//String
location.port = 0;//String
location.host = 0;//String
location.hostname = 0;//String
location.pathname = 0;//String
location.protocol = 0;//String
location.port = 0;//String
location.search = 0;//String
location.target = 0;//String
location = {};
location.reload = function() {};//void
location.replace = function(url) {};//void
location.prototype = new Object();

XSLTProcessor = function() {};
XSLTProcessor.constructor = 0;//Gecko,Object
XSLTProcessor.importStylesheet = function(domNode) {};//Gecko,Object
XSLTProcessor.transformToDocument = function(domNode) {};//Gecko,Document
XSLTProcessor.transformToFragment = function(domToBeTransformed,ownerDocument) {};//Gecko,DocumentFragment
XSLTProcessor.prototype = new Object();

DOMParser = function() {};
DOMParser.constructor = 0;//Gecko,Object
DOMParser.parseFromString = function(stringToParse,contentType) {};//Gecko,Document
DOMParser.prototype = new Object();

XMLSerializer = function() {};
XMLSerializer.constructor = 0;//Gecko,Object
XMLSerializer.serializeToStream = function(domNode,stream,encoding) {};//Gecko,Object
XMLSerializer.serializeToString = function(domNode) {};//Gecko,String
XMLSerializer.prototype = new Object();

Event.data = 0;//Object[]
Event.height = 0;//Number
Event.x = 0;//IE,Number
Event.screenX = 0;//Number
Event.clientX = 0;//Number
Event.offsetX = 0;//IE,Number
Event.layerX = 0;//Gecko,Number
Event.fromElement = 0;//IE,Object
Event.wheelDelta = 0;//IE,Number
Event.repeat = 0;//IE,Boolean
Event.pageX = 0;//Number
Event.y = 0;//IE,Number
Event.screenY = 0;//Number
Event.clientY = 0;//Number
Event.offsetY = 0;//IE,Number
Event.layerY = 0;//Gecko,Number
Event.pageY = 0;//Number
Event.width = 0;//Number
Event.modifiers = 0;//Number
Event.keyCode = 0;//Number
Event.type = 0;//String
Event.which = 0;//Object
Event.srcElement = 0;//IE,Object
Event.toElement = 0;//IE,Object
Event.cancelBubble = 0;//IE,Boolean
Event.returnValue = 0;//IE,Boolean
Event.altKey = 0;//Boolean
Event.ctrlKey = 0;//Boolean
Event.shiftKey = 0;//Boolean
Event.button = 0;//Boolean
Event.charCode = 0;//Gecko,String
Event.ABORT = 0;//Gecko,String,deprecated
Event.BLUR = 0;//Gecko,String,deprecated
Event.CLICK = 0;//Gecko,String,deprecated
Event.CHANGE = 0;//Gecko,String,deprecated
Event.DBLCLICK = 0;//Gecko,String,deprecated
Event.DRAGDROP = 0;//Gecko,String,deprecated
Event.ERROR = 0;//Gecko,String,deprecated
Event.FOCUS = 0;//Gecko,String,deprecated
Event.KEYDOWN = 0;//Gecko,String,deprecated
Event.KEYPRESS = 0;//Gecko,String,deprecated
Event.KEYUP = 0;//Gecko,String,deprecated
Event.LOAD = 0;//Gecko,String,deprecated
Event.MOUSEDOWN = 0;//Gecko,String,deprecated
Event.MOUSEMOVE = 0;//Gecko,String,deprecated
Event.MOUSEOUT = 0;//Gecko,String,deprecated
Event.MOUSEOVER = 0;//Gecko,String,deprecated
Event.MOUSEUP = 0;//Gecko,String,deprecated
Event.MOVE = 0;//Gecko,String,deprecated
Event.RESET = 0;//Gecko,String,deprecated
Event.RESIZE = 0;//Gecko,String,deprecated
Event.SELECT = 0;//Gecko,String,deprecated
Event.SUBMIT = 0;//Gecko,String,deprecated
Event.UNLOAD = 0;//Gecko,String,deprecated
Event = {};
Event.prototype = new Object();

Selection.anchorNode = 0;//Gecko,Node
Selection.anchorOffset = 0;//Gecko,Number
Selection.focusNode = 0;//Gecko,Node
Selection.focusOffset = 0;//Gecko,Number
Selection.isCollapsed = 0;//Gecko,Boolean
Selection.rangeCount = 0;//Gecko,Number
Selection.type = 0;//IE,Object
Selection = {};
Selection.getRangeAt = function(index) {};//Gecko,Range
Selection.collapse = function(parentNode,offset) {};//Gecko,void
Selection.extend = function(parentNode,offset) {};//Gecko,void
Selection.collapseToStart = function() {};//Gecko,void
Selection.collapseToEnd = function() {};//Gecko,void
Selection.selectAllChildren = function(parentNode) {};//Gecko,void
Selection.addRange = function(range) {};//Gecko,void
Selection.removeRange = function(range) {};//Gecko,void
Selection.removeAllRanges = function() {};//Gecko,void
Selection.containsNode = function(node,partlyContained) {};//Gecko,Boolean
Selection.clear = function() {};//IE,Boolean
Selection.empty = function() {};//IE,Boolean
Selection.createRange = function() {};//IE,TextRange
Selection.prototype = new Object();

TextRange.htmlText = 0;//IE,String
TextRange.text = 0;//IE,String
TextRange.boundingWidth = 0;//IE,Number
TextRange.boundingHeight = 0;//IE,Number
TextRange.boundingLeft = 0;//IE,Number
TextRange.offsetLeft = 0;//IE,Number
TextRange.boundingTop = 0;//IE,Number
TextRange.offsetTop = 0;//IE,Number
TextRange = {};
TextRange.collapse = function(start) {};//IE,void
TextRange.expand = function(unit) {};//IE,void
TextRange.duplicate = function() {};//IE,TextRange
TextRange.select = function() {};//IE,void
TextRange.parentElement = function() {};//IE,Node
TextRange.inRange = function(other) {};//IE,boolean
TextRange.isEqual = function(other) {};//IE,boolean
TextRange.scrollIntoView = function(start) {};//IE,void
TextRange.setEndPoint = function(type,range) {};//IE,void
TextRange.compareEndPoint = function(type,range) {};//IE,void
TextRange.move = function(unit,count) {};//IE,void
TextRange.moveStart = function(unit,count) {};//IE,void
TextRange.moveEnd = function(unit,count) {};//IE,void
TextRange.pasteHTML = function(htmlText) {};//IE,void
TextRange.moveToElementText = function(element) {};//IE,void
TextRange.compareEndPoints = function(sType,oRange) {};//IE,Number
TextRange.prototype = new Object();

document.selection = 0;//IE,Selection
document.namespaces = 0;//IE,Array
document.all = 0;//IE,All
document.styleSheets = 0;//Stylesheet[]
document = {};
document.execCommand = function(sCommand,bUserInterface,vValue) {};//IE,Boolean
document.prototype = new Object();

CssRule.selectorText = 0;//String
CssRule = {};
CssRule.prototype = new Object();

Stylesheet.cssRules = 0;//Gecko,CssRule[]
Stylesheet.rules = 0;//IE,CssRule[]
Stylesheet.owningElement = 0;//IE,HtmlElement
Stylesheet = {};
Stylesheet.addRule = function(selector,style) {};//IE,void
Stylesheet.insertRule = function(ruleText,index) {};//Gecko,void
Stylesheet.removeRule = function(index) {};//IE,void
Stylesheet.deleteRule = function(index) {};//Gecko,void
Stylesheet.prototype = new Object();

MozillaSideBar.addPanel = function(title,url,param3) {};//Gecko,void
MozillaSideBar.prototype = new Object();

IEExternal.AddFavorite = function(title,url) {};//IE,void
IEExternal.prototype = new Object();

history.back = function() {};//void
history.forward = function() {};//void
history.go = function(count) {};//void
history.prototype = new Object();

window.document = 0;//HTMLDocument
window.event = 0;//Event
window.navigator = 0;//Navigator
window.screen = 0;//Screen
window.location = 0;//Location
window.frameElement = 0;//Object
window.opener = 0;//window
window.window = 0;//window
window.parent = 0;//window
window.top = 0;//window
window.self = 0;//Object
window.frames = 0;//Array
window.innerHeight = 0;//Number
window.innerWidth = 0;//Number
window.outerHeight = 0;//Number
window.outerWidth = 0;//Number
window.screenLeft = 0;//IE,Number
window.screenTop = 0;//IE,Number
window.scrollX = 0;//Number
window.scrollY = 0;//Number
window.pageXOffset = 0;//Number
window.pageYOffset = 0;//Number
window.scrollMaxX = 0;//Number
window.scrollMaxY = 0;//Number
window.status = 0;//String
window.defaultStatus = 0;//String
window.toolbar = 0;//Object
window.menubar = 0;//Object
window.scrollbars = 0;//Object
window.directories = 0;//Object
window.history = 0;//history
window.sidebar = 0;//Gecko,MozillaSideBar
window.external = 0;//IE,IEExternal
window.opera = 0;//Opera,Boolean
window = {};
window.getSelection = function() {};//Gecko,Selection
window.print = function() {};//void
window.alert = function(sMesssage) {};//Object
window.confirm = function(sMesssage) {};//Object
window.prompt = function(sMessage,sDefaultValue) {};//Object
window.clearInterval = function(intervalId) {};//Object
window.clearTimeout = function(intervalId) {};//Object
window.setInterval = function(vCode,iMillis) {};//Number
window.setTimeout = function(vCode,iMillis) {};//Number
window.captureEvents = function(eventType) {};//Gecko,void,deprecated
window.releaseEvents = function(eventType) {};//Gecko,void,deprecated
window.routeEvent = function(event) {};//Gecko,void,deprecated
window.preventBubble = function(event) {};//Gecko,void,deprecated
window.GetObject = function(classOrPath,class) {};//IE,Object,deprecated
window.ScriptEngine = function() {};//IE,String
window.ScriptEngineBuildVersion = function() {};//IE,String
window.ScriptEngineMajorVersion = function() {};//IE,String
window.ScriptEngineMinorVersion = function() {};//IE,String
window.scrollTo = function(x,y) {};//Object
window.scrollBy = function(xDelta,yDelta) {};//Object
window.moveTo = function(x,y) {};//Object
window.moveBy = function(xDelta,yDelta) {};//Object
window.resizeTo = function(width,height) {};//Object
window.resizeBy = function(widthDelta,heightDelta) {};//Object
window.getComputedStyle = function(element,pseudoElt) {};//Gecko,style
window.showModalDialog = function(sUrl,vArguments,sFeatures) {};//IE,style
window.attachEvent = function(type,listener) {};//IE,Object
window.detachEvent = function(type,listener) {};//IE,Object
window.addEventListener = function(type,listener,useCapture) {};//Gecko,Object
window.removeEventListener = function(type,listener,useCapture) {};//Gecko,Object
window.execScript = function(sScript) {};//IE,Object
window.CollectGarbage = function() {};//IE,Object
window.prototype = new Object();

window = 0;//window
escape = function() {};//Object,deprecated
unescape = function() {};//Object,deprecated

HTMLCollection.length = 0;//Number
HTMLCollection = {};
HTMLCollection.item = function(index) {};//Node
HTMLCollection.namedItem = function(name) {};//Node
HTMLCollection.prototype = new Object();

HTMLOptionsCollection.length = 0;//Number
HTMLOptionsCollection = {};
HTMLOptionsCollection.item = function(index) {};//Node
HTMLOptionsCollection.namedItem = function(name) {};//Node
HTMLOptionsCollection.prototype = new Object();

HTMLDocument.title = 0;//String
HTMLDocument.referrer = 0;//String
HTMLDocument.domain = 0;//String
HTMLDocument.URL = 0;//String
HTMLDocument.body = 0;//HTMLElement
HTMLDocument.images = 0;//HTMLCollection
HTMLDocument.applets = 0;//HTMLCollection
HTMLDocument.links = 0;//HTMLCollection
HTMLDocument.forms = 0;//HTMLCollection
HTMLDocument.anchors = 0;//HTMLCollection
HTMLDocument.cookie = 0;//HTMLCollection
HTMLDocument.defaultView = 0;//Gecko,DocumentView
HTMLDocument.activeElement = 0;//IE,DocumentView
HTMLDocument.compatMode = 0;//String
HTMLDocument.parentWindow = 0;//IE,window
HTMLDocument.commandDispatcher = 0;//Gecko,CommandDispatcher
HTMLDocument = {};
HTMLDocument.open = function() {};//Object
HTMLDocument.close = function() {};//Object
HTMLDocument.write = function(text) {};//Object
HTMLDocument.writeln = function(text) {};//Object
HTMLDocument.getElementsByName = function(elementName) {};//NodeList
HTMLDocument.getElementsByClassName = function(className) {};//Gecko,NodeList
HTMLDocument.createStyleSheet = function() {};//IE,Stylesheet
HTMLDocument.getBoxObjectFor = function(element) {};//Gecko,Node
HTMLDocument.querySelectorAll = function(string) {};//Gecko,NodeList
HTMLDocument.querySelector = function(string) {};//Gecko,Node
HTMLDocument.getAnonymousNodes = function(node) {};//Gecko,Node[]
HTMLDocument.getAnonymousElementByAttribute = function(node,attrName,attrValue) {};//Gecko,NodeList
HTMLDocument.elementFromPoint = function(iX,iY) {};//IE,HTMLElement
HTMLDocument.createEventObject = function(oExistingEvent) {};//IE,Even
HTMLDocument.prototype = new Document();

CommandDispatcher.focusedElement = 0;//Gecko,HTMLElement
CommandDispatcher = {};
CommandDispatcher.prototype = new Object();

DocumentView.getComputedStyle = function(element,type) {};//Gecko,CssStyle
DocumentView.prototype = new Object();

CssStyle.getPropertyValue = function(propertyName) {};//Gecko,String
CssStyle.prototype = new Object();

HTMLElement.id = 0;//String
HTMLElement.title = 0;//String
HTMLElement.lang = 0;//String
HTMLElement.dir = 0;//String
HTMLElement.className = 0;//String
HTMLElement.children = 0;//IE,HTMLElement[]
HTMLElement.style = 0;//style
HTMLElement.clientWidth = 0;//Number
HTMLElement.clientHeight = 0;//Number
HTMLElement.clientTop = 0;//Number
HTMLElement.clientLeft = 0;//Number
HTMLElement.innerHTML = 0;//String
HTMLElement.innerText = 0;//IE,String
HTMLElement.offsetWidth = 0;//Number
HTMLElement.offsetHeight = 0;//Number
HTMLElement.offsetTop = 0;//Number
HTMLElement.offsetLeft = 0;//Number
HTMLElement.offsetParent = 0;//Number
HTMLElement.scrollWidth = 0;//Number
HTMLElement.scrollHeight = 0;//Number
HTMLElement.scrollTop = 0;//Number
HTMLElement.scrollLeft = 0;//Number
HTMLElement.stylesheet = 0;//Stylesheet
HTMLElement.currentStyle = 0;//IE,IEElementStyle
HTMLElement.runtimeStyle = 0;//IE,IEElementStyle
HTMLElement.filters = 0;//IE,Array
HTMLElement.boxObject = 0;//Gecko,HTMLElement
HTMLElement.propertyName = 0;//IE,String
HTMLElement.isDisabled = 0;//IE,Boolean
HTMLElement = {};
HTMLElement.insertAdjacentHTML = function(position,htmlContent) {};//IE,Object
HTMLElement.insertAdjacentText = function(position,textContent) {};//IE,Object
HTMLElement.attachEvent = function(type,listener) {};//IE,Object
HTMLElement.detachEvent = function(type,listener) {};//IE,Object
HTMLElement.fireEvent = function(type,event) {};//IE,Object
HTMLElement.addBehavior = function(sUrl) {};//IE,Number
HTMLElement.removeBehavior = function(sID) {};//IE,Boolean
HTMLElement.getBoundingClientRect = function() {};//IE,TextRange
HTMLElement.showPopup = function() {};//Gecko,Object
HTMLElement.hidePopup = function() {};//Gecko,Object
HTMLElement.setCapture = function(bContainerCapture) {};//IE,Object
HTMLElement.releaseCapture = function() {};//IE,Object
HTMLElement.clearAttributes = function() {};//IE,Object
HTMLElement.mergeAttributes = function(oSource,bPreserve) {};//IE,Object
var HTMLElement = {
	onblur: function () {},
	onclick: function () {},
	ondblclick: function () {},
	onfocus: function () {},
	onkeydown: function () {},
	onkeyup: function () {},
	onmouseup: function () {},
	onmousedown: function () {},
	onmouseout: function () {},
	onmouseover: function () {},
	onmousemove: function () {},
	onresize: function () {}
};
HTMLElement.prototype = new Element();

IEElementStyle.hasLayout = 0;//IE,Boolean
IEElementStyle = {};
IEElementStyle.prototype = new style();

HTMLHtmlElement.version = 0;//String
HTMLHtmlElement = {};
HTMLHtmlElement.prototype = new HTMLElement();

HTMLHeadElement.profile = 0;//String
HTMLHeadElement = {};
HTMLHeadElement.prototype = new HTMLElement();

HTMLLinkElement.disabled = 0;//Boolean
HTMLLinkElement.charset = 0;//String
HTMLLinkElement.href = 0;//String
HTMLLinkElement.hreflang = 0;//String
HTMLLinkElement.media = 0;//String
HTMLLinkElement.rel = 0;//String
HTMLLinkElement.rev = 0;//String
HTMLLinkElement.target = 0;//String
HTMLLinkElement.type = 0;//String
HTMLLinkElement = {};
HTMLLinkElement.prototype = new HTMLElement();

HTMLTitleElement.text = 0;//String
HTMLTitleElement = {};
HTMLTitleElement.prototype = new HTMLElement();

HTMLMetaElement.content = 0;//String
HTMLMetaElement.httpEquiv = 0;//String
HTMLMetaElement.name = 0;//String
HTMLMetaElement.scheme = 0;//String
HTMLMetaElement = {};
HTMLMetaElement.prototype = new HTMLElement();

HTMLBaseElement.href = 0;//String
HTMLBaseElement.target = 0;//String
HTMLBaseElement = {};
HTMLBaseElement.prototype = new HTMLElement();

HTMLIsIndexElement.form = 0;//HTMLFormElement
HTMLIsIndexElement.prompt = 0;//String
HTMLIsIndexElement = {};
HTMLIsIndexElement.prototype = new HTMLElement();

HTMLStyleElement.disabled = 0;//Boolean
HTMLStyleElement.media = 0;//String
HTMLStyleElement.type = 0;//String
HTMLStyleElement.styleSheet = 0;//IE,Stylesheet
HTMLStyleElement = {};
HTMLStyleElement.prototype = new HTMLElement();

HTMLBodyElement.aLink = 0;//String
HTMLBodyElement.background = 0;//String
HTMLBodyElement.bgColor = 0;//String
HTMLBodyElement.link = 0;//String
HTMLBodyElement.text = 0;//String
HTMLBodyElement.vLink = 0;//String
HTMLBodyElement = {};
var HTMLBodyElement = {
	onload: function () {},
	onunload: function () {}
};
HTMLBodyElement.prototype = new HTMLElement();

HTMLFormElement.elements = 0;//HTMLCollection
HTMLFormElement.length = 0;//Number
HTMLFormElement.name = 0;//String
HTMLFormElement.acceptCharset = 0;//String
HTMLFormElement.action = 0;//String
HTMLFormElement.enctype = 0;//String
HTMLFormElement.method = 0;//String
HTMLFormElement.target = 0;//String
HTMLFormElement = {};
HTMLFormElement.submit = function() {};//Object
HTMLFormElement.reset = function() {};//Object
HTMLFormElement.prototype = new Object();

HTMLSelectElement.type = 0;//String
HTMLSelectElement.selectedIndex = 0;//Number
HTMLSelectElement.value = 0;//String
HTMLSelectElement.length = 0;//Number
HTMLSelectElement.form = 0;//HTMLFormElement
HTMLSelectElement.options = 0;//HTMLOptionsCollection
HTMLSelectElement.disabled = 0;//Boolean
HTMLSelectElement.multiple = 0;//Boolean
HTMLSelectElement.name = 0;//String
HTMLSelectElement.size = 0;//Number
HTMLSelectElement.tabIndex = 0;//Number
HTMLSelectElement = {};
HTMLSelectElement.add = function(element,before) {};//Object
HTMLSelectElement.remove = function(index) {};//Object
HTMLSelectElement.blur = function() {};//Object
HTMLSelectElement.focus = function() {};//Object
HTMLSelectElement.prototype = new HTMLElement();

HTMLOptGroupElement.disabled = 0;//Boolean
HTMLOptGroupElement.label = 0;//String
HTMLOptGroupElement = {};
HTMLOptGroupElement.prototype = new HTMLElement();

HTMLOptionElement.form = 0;//HTMLFormElement
HTMLOptionElement.defaultSelected = 0;//Boolean
HTMLOptionElement.text = 0;//String
HTMLOptionElement.index = 0;//Number
HTMLOptionElement.disabled = 0;//Boolean
HTMLOptionElement.label = 0;//String
HTMLOptionElement.selected = 0;//Boolean
HTMLOptionElement.value = 0;//String
HTMLOptionElement = {};
HTMLOptionElement.prototype = new HTMLElement();

HTMLInputElement.defaultValue = 0;//String
HTMLInputElement.defaultChecked = 0;//Boolean
HTMLInputElement.form = 0;//HTMLFormElement
HTMLInputElement.accept = 0;//String
HTMLInputElement.accessKey = 0;//String
HTMLInputElement.align = 0;//String
HTMLInputElement.alt = 0;//String
HTMLInputElement.checked = 0;//Boolean
HTMLInputElement.disabled = 0;//Boolean
HTMLInputElement.maxLength = 0;//Number
HTMLInputElement.name = 0;//String
HTMLInputElement.readOnly = 0;//Boolean
HTMLInputElement.size = 0;//Number
HTMLInputElement.src = 0;//String
HTMLInputElement.tabIndex = 0;//Number
HTMLInputElement.type = 0;//String
HTMLInputElement.useMap = 0;//String
HTMLInputElement.value = 0;//String
HTMLInputElement = {};
HTMLInputElement.blur = function() {};//Object
HTMLInputElement.focus = function() {};//Object
HTMLInputElement.select = function() {};//Object
HTMLInputElement.click = function() {};//Object
HTMLInputElement.setSelectionRange = function(start,end) {};//Gecko,Object
HTMLInputElement.createTextRange = function() {};//IE,TextRange
HTMLInputElement.prototype = new HTMLElement();

HTMLTextAreaElement.defaultValue = 0;//String
HTMLTextAreaElement.form = 0;//HTMLFormElement
HTMLTextAreaElement.accessKey = 0;//String
HTMLTextAreaElement.cols = 0;//Number
HTMLTextAreaElement.disabled = 0;//Boolean
HTMLTextAreaElement.name = 0;//String
HTMLTextAreaElement.readOnly = 0;//Boolean
HTMLTextAreaElement.rows = 0;//Number
HTMLTextAreaElement.tabIndex = 0;//Number
HTMLTextAreaElement.type = 0;//String
HTMLTextAreaElement.value = 0;//String
HTMLTextAreaElement = {};
HTMLTextAreaElement.blur = function() {};//Object
HTMLTextAreaElement.focus = function() {};//Object
HTMLTextAreaElement.select = function() {};//Object
HTMLTextAreaElement.prototype = new HTMLElement();

HTMLButtonElement.form = 0;//HTMLFormElement
HTMLButtonElement.accessKey = 0;//String
HTMLButtonElement.disabled = 0;//Boolean
HTMLButtonElement.name = 0;//String
HTMLButtonElement.tabIndex = 0;//Number
HTMLButtonElement.type = 0;//String
HTMLButtonElement.value = 0;//String
HTMLButtonElement = {};
HTMLButtonElement.prototype = new HTMLElement();

HTMLLabelElement.form = 0;//HTMLFormElement
HTMLLabelElement.accessKey = 0;//String
HTMLLabelElement.htmlFor = 0;//
HTMLLabelElement = {};
HTMLLabelElement.prototype = new HTMLElement();

HTMLFieldSetElement.form = 0;//HTMLFormElement
HTMLFieldSetElement = {};
HTMLFieldSetElement.prototype = new HTMLElement();

HTMLLegendElement.form = 0;//HTMLFormElement
HTMLLegendElement.accessKey = 0;//String
HTMLLegendElement.align = 0;//String
HTMLLegendElement = {};
HTMLLegendElement.prototype = new HTMLElement();

HTMLUListElement.compact = 0;//Boolean
HTMLUListElement.type = 0;//String
HTMLUListElement = {};
HTMLUListElement.prototype = new HTMLElement();

HTMLOListElement.compact = 0;//Boolean
HTMLOListElement.start = 0;//Number
HTMLOListElement.type = 0;//String
HTMLOListElement = {};
HTMLOListElement.prototype = new HTMLElement();

HTMLDListElement.compact = 0;//Boolean
HTMLDListElement = {};
HTMLDListElement.prototype = new HTMLElement();

HTMLDirectoryElement.compact = 0;//Boolean
HTMLDirectoryElement = {};
HTMLDirectoryElement.prototype = new HTMLElement();

HTMLMenuElement.compact = 0;//Boolean
HTMLMenuElement = {};
HTMLMenuElement.prototype = new HTMLElement();

HTMLLIElement.type = 0;//String
HTMLLIElement.value = 0;//Number
HTMLLIElement = {};
HTMLLIElement.prototype = new HTMLElement();

HTMLDivElement.align = 0;//String
HTMLDivElement = {};
HTMLDivElement.prototype = new HTMLElement();

HTMLParagraphElement.align = 0;//String
HTMLParagraphElement = {};
HTMLParagraphElement.prototype = new HTMLElement();

HTMLHeadingElement.align = 0;//String
HTMLHeadingElement = {};
HTMLHeadingElement.prototype = new HTMLElement();

HTMLQuoteElement.cite = 0;//String
HTMLQuoteElement = {};
HTMLQuoteElement.prototype = new HTMLElement();

HTMLPreElement.width = 0;//Number
HTMLPreElement = {};
HTMLPreElement.prototype = new HTMLElement();

HTMLBRElement.clear = 0;//String
HTMLBRElement = {};
HTMLBRElement.prototype = new HTMLElement();

HTMLBaseFontElement.color = 0;//String
HTMLBaseFontElement.face = 0;//String
HTMLBaseFontElement.size = 0;//Number
HTMLBaseFontElement = {};
HTMLBaseFontElement.prototype = new HTMLElement();

HTMLFontElement.color = 0;//String
HTMLFontElement.face = 0;//String
HTMLFontElement.size = 0;//String
HTMLFontElement = {};
HTMLFontElement.prototype = new HTMLElement();

HTMLHRElement.align = 0;//String
HTMLHRElement.noShade = 0;//Boolean
HTMLHRElement.size = 0;//String
HTMLHRElement.width = 0;//String
HTMLHRElement = {};
HTMLHRElement.prototype = new HTMLElement();

HTMLModElement.cite = 0;//String
HTMLModElement.dateTime = 0;//String
HTMLModElement = {};
HTMLModElement.prototype = new HTMLElement();

HTMLAnchorElement.accessKey = 0;//String
HTMLAnchorElement.charset = 0;//String
HTMLAnchorElement.coords = 0;//String
HTMLAnchorElement.href = 0;//String
HTMLAnchorElement.hreflang = 0;//String
HTMLAnchorElement.name = 0;//String
HTMLAnchorElement.rel = 0;//String
HTMLAnchorElement.rev = 0;//String
HTMLAnchorElement.shape = 0;//String
HTMLAnchorElement.tabIndex = 0;//Number
HTMLAnchorElement.target = 0;//String
HTMLAnchorElement.type = 0;//String
HTMLAnchorElement = {};
HTMLAnchorElement.blur = function() {};//Object
HTMLAnchorElement.focus = function() {};//Object
HTMLAnchorElement.prototype = new HTMLElement();

HTMLImageElement.name = 0;//String
HTMLImageElement.align = 0;//String
HTMLImageElement.alt = 0;//String
HTMLImageElement.border = 0;//String
HTMLImageElement.height = 0;//Number
HTMLImageElement.hspace = 0;//Number
HTMLImageElement.isMap = 0;//Boolean
HTMLImageElement.longDesc = 0;//String
HTMLImageElement.src = 0;//String
HTMLImageElement.useMap = 0;//String
HTMLImageElement.vspace = 0;//Number
HTMLImageElement.width = 0;//Number
HTMLImageElement = {};
HTMLImageElement.prototype = new HTMLElement();

HTMLObjectElement.form = 0;//HTMLFormElement
HTMLObjectElement.code = 0;//String
HTMLObjectElement.align = 0;//String
HTMLObjectElement.archive = 0;//String
HTMLObjectElement.border = 0;//String
HTMLObjectElement.codeBase = 0;//String
HTMLObjectElement.codeType = 0;//String
HTMLObjectElement.data = 0;//String
HTMLObjectElement.declare = 0;//Boolean
HTMLObjectElement.height = 0;//String
HTMLObjectElement.hspace = 0;//Number
HTMLObjectElement.name = 0;//String
HTMLObjectElement.standby = 0;//String
HTMLObjectElement.tabIndex = 0;//Number
HTMLObjectElement.type = 0;//String
HTMLObjectElement.useMap = 0;//String
HTMLObjectElement.vspace = 0;//Number
HTMLObjectElement.width = 0;//String
HTMLObjectElement.contentDocument = 0;//Document
HTMLObjectElement = {};
HTMLObjectElement.prototype = new HTMLElement();

HTMLParamElement.name = 0;//String
HTMLParamElement.type = 0;//String
HTMLParamElement.value = 0;//String
HTMLParamElement.valueType = 0;//String
HTMLParamElement = {};
HTMLParamElement.prototype = new HTMLElement();

HTMLAppletElement.align = 0;//String
HTMLAppletElement.alt = 0;//String
HTMLAppletElement.archive = 0;//String
HTMLAppletElement.code = 0;//String
HTMLAppletElement.codeBase = 0;//String
HTMLAppletElement.height = 0;//String
HTMLAppletElement.hspace = 0;//Number
HTMLAppletElement.name = 0;//String
HTMLAppletElement.object = 0;//String
HTMLAppletElement.vspace = 0;//Number
HTMLAppletElement.width = 0;//String
HTMLAppletElement = {};
HTMLAppletElement.prototype = new HTMLElement();

HTMLMapElement.areas = 0;//HTMLCollection
HTMLMapElement.name = 0;//String
HTMLMapElement = {};
HTMLMapElement.prototype = new HTMLElement();

HTMLAreaElement.accessKey = 0;//String
HTMLAreaElement.alt = 0;//String
HTMLAreaElement.coords = 0;//String
HTMLAreaElement.href = 0;//String
HTMLAreaElement.noHref = 0;//Boolean
HTMLAreaElement.shape = 0;//String
HTMLAreaElement.tabIndex = 0;//Number
HTMLAreaElement.target = 0;//String
HTMLAreaElement = {};
HTMLAreaElement.prototype = new HTMLElement();

HTMLScriptElement.text = 0;//String
HTMLScriptElement.htmlFor = 0;//String
HTMLScriptElement.event = 0;//String
HTMLScriptElement.charset = 0;//String
HTMLScriptElement.defer = 0;//Boolean
HTMLScriptElement.src = 0;//String
HTMLScriptElement.type = 0;//String
HTMLScriptElement = {};
HTMLScriptElement.prototype = new HTMLElement();

HTMLTableElement.caption = 0;//HTMLTableCaptionElement
HTMLTableElement.tHead = 0;//HTMLTableSectionElement
HTMLTableElement.tFoot = 0;//HTMLTableSectionElement
HTMLTableElement.rows = 0;//HTMLCollection
HTMLTableElement.tBodies = 0;//HTMLCollection
HTMLTableElement.align = 0;//String
HTMLTableElement.bgColor = 0;//String
HTMLTableElement.border = 0;//String
HTMLTableElement.cellPadding = 0;//String
HTMLTableElement.cellSpacing = 0;//String
HTMLTableElement.frame = 0;//String
HTMLTableElement.rules = 0;//String
HTMLTableElement.summary = 0;//String
HTMLTableElement.width = 0;//String
HTMLTableElement = {};
HTMLTableElement.createTHead = function() {};//HTMLElement
HTMLTableElement.deleteTHead = function() {};//Object
HTMLTableElement.createTFoot = function() {};//HTMLElement
HTMLTableElement.deleteTFoot = function() {};//Object
HTMLTableElement.createCaption = function() {};//HTMLElement
HTMLTableElement.deleteCaption = function() {};//Object
HTMLTableElement.insertRow = function(index) {};//HTMLElement
HTMLTableElement.deleteRow = function(index) {};//Object
HTMLTableElement.prototype = new HTMLElement();

HTMLTableCaptionElement.align = 0;//String
HTMLTableCaptionElement = {};
HTMLTableCaptionElement.prototype = new HTMLElement();

HTMLTableColElement.align = 0;//String
HTMLTableColElement.ch = 0;//String
HTMLTableColElement.chOff = 0;//String
HTMLTableColElement.span = 0;//Number
HTMLTableColElement.vAlign = 0;//String
HTMLTableColElement.width = 0;//String
HTMLTableColElement = {};
HTMLTableColElement.prototype = new HTMLElement();

HTMLTableSectionElement.align = 0;//String
HTMLTableSectionElement.ch = 0;//String
HTMLTableSectionElement.chOff = 0;//String
HTMLTableSectionElement.vAlign = 0;//String
HTMLTableSectionElement.rows = 0;//HTMLCollection
HTMLTableSectionElement = {};
HTMLTableSectionElement.insertRow = function(index) {};//HTMLElement
HTMLTableSectionElement.deleteRow = function(index) {};//Object
HTMLTableSectionElement.prototype = new HTMLElement();

HTMLTableRowElement.rowIndex = 0;//Number
HTMLTableRowElement.sectionRowIndex = 0;//Number
HTMLTableRowElement.cells = 0;//HTMLCollection
HTMLTableRowElement.align = 0;//String
HTMLTableRowElement.bgColor = 0;//String
HTMLTableRowElement.ch = 0;//String
HTMLTableRowElement.chOff = 0;//String
HTMLTableRowElement.vAlign = 0;//String
HTMLTableRowElement = {};
HTMLTableRowElement.insertCell = function(index) {};//HTMLElement
HTMLTableRowElement.deleteCell = function(index) {};//Object
HTMLTableRowElement.prototype = new HTMLElement();

HTMLTableCellElement.cellIndex = 0;//Number
HTMLTableCellElement.abbr = 0;//String
HTMLTableCellElement.align = 0;//String
HTMLTableCellElement.axis = 0;//String
HTMLTableCellElement.bgColor = 0;//String
HTMLTableCellElement.ch = 0;//String
HTMLTableCellElement.chOff = 0;//String
HTMLTableCellElement.colSpan = 0;//Number
HTMLTableCellElement.headers = 0;//String
HTMLTableCellElement.height = 0;//String
HTMLTableCellElement.noWrap = 0;//Boolean
HTMLTableCellElement.rowSpan = 0;//Number
HTMLTableCellElement.scope = 0;//String
HTMLTableCellElement.vAlign = 0;//String
HTMLTableCellElement.width = 0;//String
HTMLTableCellElement = {};
HTMLTableCellElement.prototype = new HTMLElement();

HTMLFrameSetElement.cols = 0;//String
HTMLFrameSetElement.rows = 0;//String
HTMLFrameSetElement = {};
HTMLFrameSetElement.prototype = new HTMLElement();

HTMLFrameElement.frameBorder = 0;//String
HTMLFrameElement.longDesc = 0;//String
HTMLFrameElement.marginHeight = 0;//String
HTMLFrameElement.marginWidth = 0;//String
HTMLFrameElement.name = 0;//String
HTMLFrameElement.noResize = 0;//Boolean
HTMLFrameElement.scrolling = 0;//String
HTMLFrameElement.src = 0;//String
HTMLFrameElement.contentDocument = 0;//Gecko,Document
HTMLFrameElement = {};
HTMLFrameElement.prototype = new HTMLElement();

HTMLIFrameElement.align = 0;//String
HTMLIFrameElement.frameBorder = 0;//String
HTMLIFrameElement.height = 0;//String
HTMLIFrameElement.longDesc = 0;//String
HTMLIFrameElement.marginHeight = 0;//String
HTMLIFrameElement.marginWidth = 0;//String
HTMLIFrameElement.name = 0;//String
HTMLIFrameElement.scrolling = 0;//String
HTMLIFrameElement.src = 0;//String
HTMLIFrameElement.width = 0;//String
HTMLIFrameElement.contentDocument = 0;//Document
HTMLIFrameElement.contentWindow = 0;//window
HTMLIFrameElement = {};
HTMLIFrameElement.prototype = new HTMLElement();
