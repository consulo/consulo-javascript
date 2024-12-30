__defineGetter__ = function (propertyName, func) {}; //Gecko,Object
__defineSetter__ = function (propertyName, func) {}; //Gecko,Object
__lookupGetter__ = function (propertyName) {}; //Gecko,Function
__lookupSetter__ = function (propertyName) {}; //Gecko,Function
__proto__ = function (propertyName) {}; //Gecko,Function

var Image = function () {};
Image.prototype = new HTMLImageElement();
Image.constructor = 0; //Object
Image.naturalWidth = 0; //Gecko,Object
Image.naturalHeight = 0; //Gecko,Object

var Navigator = {
    appCodeName: 0, //String
    appName: 0, //String
    appVersion: 0, //String
    javaEnabled: function () {}, //Boolean
    language: 0, //String
    mimeTypes: 0, //Array
    platform: 0, //Array
    plugins: 0, //String
    preference: function (prefName, prefValue) {}, //String
    product: 0, //String
    systemLanguage: 0, //IE,String
    taintEnabled: function () {}, //Boolean
    userAgent: 0, //String
    userLanguage: 0, //IE,String
};

var Screen = {
    availHeight: 0, //Number
    availWidth: 0, //Number
    colorDepth: 0, //Number
    height: 0, //Number
    pixelDepth: 0, //Number
    width: 0, //Number
};

var location = {
    href: 0, //String
    hash: 0, //String
    host: 0, //String
    hostname: 0, //String
    pathname: 0, //String
    port: 0, //String
    protocol: 0, //String
    reload() {}, //void
    replace(url) {}, //void
    search: 0, //String
    target: 0, //String
};

var XSLTProcessor = function () {};
XSLTProcessor.prototype = new Object();
XSLTProcessor.constructor = 0; //Gecko,Object
XSLTProcessor.importStylesheet = function (domNode) {}; //Gecko,Object
XSLTProcessor.transformToDocument = function (domNode) {}; //Gecko,Document
XSLTProcessor.transformToFragment = function (domToBeTransformed, ownerDocument) {}; //Gecko,DocumentFragment

var DOMParser = function () {};
DOMParser.prototype = new Object();
DOMParser.constructor = 0; //Gecko,Object
DOMParser.parseFromString = function (stringToParse, contentType) {}; //Gecko,Document

var XMLSerializer = function () {};
XMLSerializer.prototype = new Object();
XMLSerializer.constructor = 0; //Gecko,Object
XMLSerializer.serializeToStream = function (domNode, stream, encoding) {}; //Gecko,Object
XMLSerializer.serializeToString = function (domNode) {}; //Gecko,String

var Event = {
    altKey: 0, //Boolean
    button: 0, //Boolean
    cancelBubble: 0, //IE,Boolean
    charCode: 0, //Gecko,String
    clientX: 0, //Number
    clientY: 0, //Number
    ctrlKey: 0, //Boolean
    data: 0, //Object[]
    height: 0, //Number
    fromElement: 0, //IE,Object
    keyCode: 0, //Number
    layerX: 0, //Gecko,Number
    layerY: 0, //Gecko,Number
    modifiers: 0, //Number
    offsetX: 0, //IE,Number
    offsetY: 0, //IE,Number
    pageX: 0, //Number
    pageY: 0, //Number
    repeat: 0, //IE,Boolean
    returnValue: 0, //IE,Boolean
    screenX: 0, //Number
    screenY: 0, //Number
    shiftKey: 0, //Boolean
    srcElement: 0, //IE,Object
    toElement: 0, //IE,Object
    type: 0, //String
    wheelDelta: 0, //IE,Number
    which: 0, //Object
    width: 0, //Number
    x: 0, //IE,Number
    y: 0, //IE,Number
    // deprecated
    ABORT: 0, //Gecko,String,deprecated
    BLUR: 0, //Gecko,String,deprecated
    CLICK: 0, //Gecko,String,deprecated
    CHANGE: 0, //Gecko,String,deprecated
    DBLCLICK: 0, //Gecko,String,deprecated
    DRAGDROP: 0, //Gecko,String,deprecated
    ERROR: 0, //Gecko,String,deprecated
    FOCUS: 0, //Gecko,String,deprecated
    KEYDOWN: 0, //Gecko,String,deprecated
    KEYPRESS: 0, //Gecko,String,deprecated
    KEYUP: 0, //Gecko,String,deprecated
    LOAD: 0, //Gecko,String,deprecated
    MOUSEDOWN: 0, //Gecko,String,deprecated
    MOUSEMOVE: 0, //Gecko,String,deprecated
    MOUSEOUT: 0, //Gecko,String,deprecated
    MOUSEOVER: 0, //Gecko,String,deprecated
    MOUSEUP: 0, //Gecko,String,deprecated
    MOVE: 0, //Gecko,String,deprecated
    RESET: 0, //Gecko,String,deprecated
    RESIZE: 0, //Gecko,String,deprecated
    SELECT: 0, //Gecko,String,deprecated
    SUBMIT: 0, //Gecko,String,deprecated
    UNLOAD: 0, //Gecko,String,deprecated
};

var Selection = {
    addRange: function (range) {}, //Gecko,void
    anchorNode: 0, //Gecko,Node
    anchorOffset: 0, //Gecko,Number
    clear: function () {}, //IE,Boolean
    collapse: function (parentNode, offset) {}, //Gecko,void
    collapseToStart: function () {}, //Gecko,void
    collapseToEnd: function () {}, //Gecko,void
    containsNode: function (node, partlyContained) {}, //Gecko,Boolean
    createRange: function () {}, //IE,TextRange
    empty: function () {}, //IE,Boolean
    extend: function (parentNode, offset) {}, //Gecko,void
    focusNode: 0, //Gecko,Node
    focusOffset: 0, //Gecko,Number
    getRangeAt: function (index) {}, //Gecko,Range
    isCollapsed: 0, //Gecko,Boolean
    rangeCount: 0, //Gecko,Number
    removeRange: function (range) {}, //Gecko,void
    removeAllRanges: function () {}, //Gecko,void
    selectAllChildren: function (parentNode) {}, //Gecko,void
    type: 0, //IE,Object
};

var TextRange = {
    boundingHeight: 0, //IE,Number
    boundingLeft: 0, //IE,Number
    boundingTop: 0, //IE,Number
    boundingWidth: 0, //IE,Number
    collapse: function (start) {}, //IE,void
    compareEndPoint: function (type, range) {}, //IE,void
    compareEndPoints: function (sType, oRange) {}, //IE,Number
    duplicate: function () {}, //IE,TextRange
    expand: function (unit) {}, //IE,void
    htmlText: 0, //IE,String
    inRange: function (other) {}, //IE,boolean
    isEqual: function (other) {}, //IE,boolean
    move: function (unit, count) {}, //IE,void
    moveEnd: function (unit, count) {}, //IE,void
    moveStart: function (unit, count) {}, //IE,void
    moveToElementText: function (element) {}, //IE,void
    offsetLeft: 0, //IE,Number
    offsetTop: 0, //IE,Number
    parentElement: function () {}, //IE,Node
    pasteHTML: function (htmlText) {}, //IE,void
    scrollIntoView: function (start) {}, //IE,void
    select: function () {}, //IE,void
    setEndPoint: function (type, range) {}, //IE,void
    text: 0, //IE,String
};

var document = {
    all: 0, //IE,All
    execCommand: function (sCommand, bUserInterface, vValue) {}, //IE,Boolean
    namespaces: 0, //IE,Array
    selection: 0, //IE,Selection
    styleSheets: 0 //Stylesheet[]
};

var CssRule = {
    selectorText: 0, //String
};

var Stylesheet = {
    addRule: function (selector, style) {}, //IE,void
    cssRules: 0, //Gecko,CssRule[]
    deleteRule: function (index) {}, //Gecko,void
    insertRule: function (ruleText, index) {}, //Gecko,void
    owningElement: 0, //IE,HtmlElement
    removeRule: function (index) {}, //IE,void
    rules: 0, //IE,CssRule[]
};

var MozillaSideBar = {
    addPanel: function (title, url, param3) {}, //Gecko,void
};

var IEExternal = {
    AddFavorite: function (title, url) {}, //IE,void
};

var history = {
    back: function () {}, //void
    forward: function () {}, //void
    go: function (count) {}, //void
};

var window = {
    addEventListener: function (type, listener, useCapture) {}, //Gecko,Object
    alert: function (sMesssage) {}, //Object
    attachEvent: function (type, listener) {}, //IE,Object
    captureEvents: function (eventType) {}, //Gecko,void,deprecated
    clearInterval: function (intervalId) {}, //Object
    clearTimeout: function (intervalId) {}, //Object
    CollectGarbage: function () {}, //IE,Object
    confirm: function (sMesssage) {}, //Object
    defaultStatus: 0, //String
    detachEvent: function (type, listener) {}, //IE,Object
    directories: 0, //Object
    document: 0, //HTMLDocument
    external: 0, //IE,IEExternal
    event: 0, //Event
    execScript: function (sScript) {}, //IE,Object
    frameElement: 0, //Object
    frames: 0, //Array
    getComputedStyle: function (element, pseudoElt) {}, //Gecko,style
    GetObject: function (classOrPath, clazz) {}, //IE,Object,deprecated
    getSelection: function () {}, //Gecko,Selection
    history: 0, //history
    innerHeight: 0, //Number
    innerWidth: 0, //Number
    location: 0, //Location
    menubar: 0, //Object
    moveBy: function (xDelta, yDelta) {}, //Object
    moveTo: function (x, y) {}, //Object
    navigator: 0, //Navigator
    opener: 0, //window
    opera: 0, //Opera,Boolean
    outerHeight: 0, //Number
    outerWidth: 0, //Number
    pageXOffset: 0, //Number
    pageYOffset: 0, //Number
    parent: 0, //window
    preventBubble: function (event) {}, //Gecko,void,deprecated
    print: function () {}, //void
    prompt: function (sMessage, sDefaultValue) {}, //Object
    releaseEvents: function (eventType) {}, //Gecko,void,deprecated
    removeEventListener: function (type, listener, useCapture) {}, //Gecko,Object
    resizeBy: function (widthDelta, heightDelta) {}, //Object
    resizeTo: function (width, height) {}, //Object
    routeEvent: function (event) {}, //Gecko,void,deprecated
    screen: 0, //Screen
    screenLeft: 0, //IE,Number
    screenTop: 0, //IE,Number
    ScriptEngine: function () {}, //IE,String
    ScriptEngineBuildVersion: function () {}, //IE,String
    ScriptEngineMajorVersion: function () {}, //IE,String
    ScriptEngineMinorVersion: function () {}, //IE,String
    scrollbars: 0, //Object
    scrollBy: function (xDelta, yDelta) {}, //Object
    scrollMaxX: 0, //Number
    scrollMaxY: 0, //Number
    scrollTo: function (x, y) {}, //Object
    scrollX: 0, //Number
    scrollY: 0, //Number
    self: 0, //Object
    setInterval: function (vCode, iMillis) {}, //Number
    setTimeout: function (vCode, iMillis) {}, //Number
    showModalDialog: function (sUrl, vArguments, sFeatures) {}, //IE,style
    sidebar: 0, //Gecko,MozillaSideBar
    status: 0, //String
    toolbar: 0, //Object
    top: 0, //window
    window: 0, //window
};

var escape = function () {}; //Object,deprecated
var unescape = function () {}; //Object,deprecated

var HTMLCollection = {
    length: 0, //Number
    item: function (index) {}, //Node
    namedItem: function (name) {}, //Node
};

var HTMLOptionsCollection = {
    item: function (index) {}, //Node
    length: 0, //Number
    namedItem: function (name) {}, //Node
};

var HTMLDocument = {
    activeElement: 0, //IE,DocumentView
    anchors: 0, //HTMLCollection
    applets: 0, //HTMLCollection
    body: 0, //HTMLElement
    close: function () {}, //Object
    commandDispatcher: 0, //Gecko,CommandDispatcher
    compatMode: 0, //String
    cookie: 0, //HTMLCollection
    createEventObject: function (oExistingEvent) {}, //IE,Even
    createStyleSheet: function () {}, //IE,Stylesheet
    defaultView: 0, //Gecko,DocumentView
    domain: 0, //String
    elementFromPoint: function (iX, iY) {}, //IE,HTMLElement
    forms: 0, //HTMLCollection
    getAnonymousElementByAttribute: function (node, attrName, attrValue) {}, //Gecko,NodeList
    getAnonymousNodes: function (node) {}, //Gecko,Node[]
    getBoxObjectFor: function (element) {}, //Gecko,Node
    getElementsByName: function (elementName) {}, //NodeList
    getElementsByClassName: function (className) {}, //Gecko,NodeList
    images: 0, //HTMLCollection
    links: 0, //HTMLCollection
    open: function () {}, //Object
    parentWindow: 0, //IE,window
    querySelector: function (string) {}, //Gecko,Node
    querySelectorAll: function (string) {}, //Gecko,NodeList
    referrer: 0, //String
    title: 0, //String
    URL: 0, //String
    write: function (text) {}, //Object
    writeln: function (text) {}, //Object
};
HTMLDocument.prototype = new Document();

var CommandDispatcher = {
    focusedElement: 0, //Gecko,HTMLElement
};

var DocumentView = {
    getComputedStyle: function (element, type) {}, //Gecko,CssStyle
};

var CssStyle = {
    getPropertyValue: function (propertyName) {}, //Gecko,String
};

var HTMLElement = {
    addBehavior: function (sUrl) {}, //IE,Number
    attachEvent: function (type, listener) {}, //IE,Object
    boxObject: 0, //Gecko,HTMLElement
    children: 0, //IE,HTMLElement[]
    className: 0, //String
    clearAttributes: function () {}, //IE,Object
    clientHeight: 0, //Number
    clientLeft: 0, //Number
    clientTop: 0, //Number
    clientWidth: 0, //Number
    currentStyle: 0, //IE,IEElementStyle
    detachEvent: function (type, listener) {}, //IE,Object
    dir: 0, //String
    filters: 0, //IE,Array
    fireEvent: function (type, event) {}, //IE,Object
    getBoundingClientRect: function () {}, //IE,TextRange
    hidePopup: function () {}, //Gecko,Object
    id: 0, //String
    innerHTML: 0, //String
    innerText: 0, //IE,String
    insertAdjacentHTML: function (position, htmlContent) {}, //IE,Object
    insertAdjacentText: function (position, textContent) {}, //IE,Object
    isDisabled: 0, //IE,Boolean
    lang: 0, //String
    mergeAttributes: function (oSource, bPreserve) {}, //IE,Object
    offsetHeight: 0, //Number
    offsetLeft: 0, //Number
    offsetParent: 0, //Number
    offsetTop: 0, //Number
    offsetWidth: 0, //Number
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
    onresize: function () {},
    propertyName: 0, //IE,String
    releaseCapture: function () {}, //IE,Object
    removeBehavior: function (sID) {}, //IE,Boolean
    runtimeStyle: 0, //IE,IEElementStyle
    setCapture: function (bContainerCapture) {}, //IE,Object
    scrollWidth: 0, //Number
    scrollHeight: 0, //Number
    scrollTop: 0, //Number
    scrollLeft: 0, //Number
    showPopup: function () {}, //Gecko,Object
    style: 0, //style
    stylesheet: 0, //Stylesheet
    title: 0, //String
};
HTMLElement.prototype = new Element();

var HTMLAnchorElement = {
    accessKey: 0, //String
    charset: 0, //String
    coords: 0, //String
    blur: function () {}, //Object
    focus: function () {}, //Object
    href: 0, //String
    hreflang: 0, //String
    name: 0, //String
    rel: 0, //String
    rev: 0, //String
    shape: 0, //String
    tabIndex: 0, //Number
    target: 0, //String
    type: 0, //String
};
HTMLAnchorElement.prototype = new HTMLElement();

var HTMLAppletElement = {
    align: 0, //String
    alt: 0, //String
    archive: 0, //String
    code: 0, //String
    codeBase: 0, //String
    height: 0, //String
    hspace: 0, //Number
    name: 0, //String
    object: 0, //String
    vspace: 0, //Number
    width: 0, //String
};
HTMLAppletElement.prototype = new HTMLElement();

var HTMLAreaElement = {
    accessKey: 0, //String
    alt: 0, //String
    coords: 0, //String
    href: 0, //String
    noHref: 0, //Boolean
    shape: 0, //String
    tabIndex: 0, //Number
    target: 0, //String
};
HTMLAreaElement.prototype = new HTMLElement();

var HTMLBaseElement = {
    href: 0, //String
    target: 0, //String
};
HTMLBaseElement.prototype = new HTMLElement();

var HTMLBaseFontElement = {
    color: 0, //String
    face: 0, //String
    size: 0, //Number
};
HTMLBaseFontElement.prototype = new HTMLElement();

var HTMLBodyElement = {
    aLink: 0, //String
    background: 0, //String
    bgColor: 0, //String
    link: 0, //String
    onload: function () {},
    onunload: function () {},
    text: 0, //String
    vLink: 0 //String
};
HTMLBodyElement.prototype = new HTMLElement();

var HTMLBRElement = {
    clear: 0 //String
};
HTMLBRElement.prototype = new HTMLElement();

var HTMLButtonElement = {
    accessKey: 0, //String
    disabled: 0, //Boolean
    form: 0, //HTMLFormElement
    name: 0, //String
    tabIndex: 0, //Number
    type: 0, //String
    value: 0, //String
};
HTMLButtonElement.prototype = new HTMLElement();

var HTMLDirectoryElement = {
    compact: 0, //Boolean
};
HTMLDirectoryElement.prototype = new HTMLElement();

var HTMLDivElement = {
    align: 0, //String
};
HTMLDivElement.prototype = new HTMLElement();

var HTMLDListElement = {
    compact: 0, //Boolean
};
HTMLDListElement.prototype = new HTMLElement();

var HTMLFieldSetElement = {
    form: 0, //HTMLFormElement
};
HTMLFieldSetElement.prototype = new HTMLElement();

var HTMLFontElement = {
    color: 0, //String
    face: 0, //String
    size: 0, //String
};
HTMLFontElement.prototype = new HTMLElement();

var HTMLFormElement = {
    acceptCharset: 0, //String
    action: 0, //String
    elements: 0, //HTMLCollection
    enctype: 0, //String
    length: 0, //Number
    method: 0, //String
    name: 0, //String
    reset: function () {}, //Object
    submit: function () {}, //Object
    target: 0, //String
};

var HTMLFrameElement = {
    contentDocument: 0, //Gecko,Document
    frameBorder: 0, //String
    longDesc: 0, //String
    marginHeight: 0, //String
    marginWidth: 0, //String
    name: 0, //String
    noResize: 0, //Boolean
    scrolling: 0, //String
    src: 0, //String
};
HTMLFrameElement.prototype = new HTMLElement();

var HTMLFrameSetElement = {
    cols: 0, //String
    rows: 0, //String
};
HTMLFrameSetElement.prototype = new HTMLElement();

var HTMLHeadElement = {
    profile: 0, //String
};
HTMLHeadElement.prototype = new HTMLElement();

var HTMLHeadingElement = {
    align: 0, //String
};
HTMLHeadingElement.prototype = new HTMLElement();

var HTMLHtmlElement = {
    version: 0, //String
};
HTMLHtmlElement.prototype = new HTMLElement();

var HTMLInputElement = {
    accept: 0, //String
    accessKey: 0, //String
    align: 0, //String
    alt: 0, //String
    blur: function () {}, //Object
    checked: 0, //Boolean
    click: function () {}, //Object
    createTextRange: function () {}, //IE,TextRange
    defaultChecked: 0, //Boolean
    defaultValue: 0, //String
    disabled: 0, //Boolean
    focus: function () {}, //Object
    form: 0, //HTMLFormElement
    maxLength: 0, //Number
    name: 0, //String
    readOnly: 0, //Boolean
    select: function () {}, //Object
    setSelectionRange: function (start, end) {}, //Gecko,Object
    size: 0, //Number
    src: 0, //String
    tabIndex: 0, //Number
    type: 0, //String
    useMap: 0, //String
    value: 0, //String
};
HTMLInputElement.prototype = new HTMLElement();

var HTMLIsIndexElement = {
    form: 0, //HTMLFormElement
    prompt: 0, //String
};
HTMLIsIndexElement.prototype = new HTMLElement();

var HTMLHRElement = {
    align: 0, //String
    noShade: 0, //Boolean
    size: 0, //String
    width: 0, //String
};
HTMLHRElement.prototype = new HTMLElement();

var HTMLIFrameElement = {
    align: 0, //String
    contentDocument: 0, //Document
    contentWindow: 0, //window
    frameBorder: 0, //String
    height: 0, //String
    longDesc: 0, //String
    marginHeight: 0, //String
    marginWidth: 0, //String
    name: 0, //String
    scrolling: 0, //String
    src: 0, //String
    width: 0, //String
};
HTMLIFrameElement.prototype = new HTMLElement();

var HTMLImageElement = {
    align: 0, //String
    alt: 0, //String
    border: 0, //String
    height: 0, //Number
    hspace: 0, //Number
    isMap: 0, //Boolean
    longDesc: 0, //String
    name: 0, //String
    src: 0, //String
    useMap: 0, //String
    vspace: 0, //Number
    width: 0, //Number
};
HTMLImageElement.prototype = new HTMLElement();

var HTMLLabelElement = {
    accessKey: 0, //String
    form: 0, //HTMLFormElement
    htmlFor: 0, //
};
HTMLLabelElement.prototype = new HTMLElement();

var HTMLLegendElement = {
    accessKey: 0, //String
    align: 0, //String
    form: 0, //HTMLFormElement
};
HTMLLegendElement.prototype = new HTMLElement();

var HTMLLIElement = {
    type: 0, //String
    value: 0, //Number
};
HTMLLIElement.prototype = new HTMLElement();

var HTMLLinkElement = {
    charset: 0, //String
    disabled: 0, //Boolean
    href: 0, //String
    hreflang: 0, //String
    media: 0, //String
    rel: 0, //String
    rev: 0, //String
    target: 0, //String
    type: 0, //String
};
HTMLLinkElement.prototype = new HTMLElement();

var HTMLMapElement = {
    areas: 0, //HTMLCollection
    name: 0, //String
};
HTMLMapElement.prototype = new HTMLElement();

var HTMLMenuElement = {
    compact: 0, //Boolean
};
HTMLMenuElement.prototype = new HTMLElement();

var HTMLMetaElement = {
    content: 0, //String
    httpEquiv: 0, //String
    name: 0, //String
    scheme: 0, //String
};
HTMLMetaElement.prototype = new HTMLElement();

var HTMLModElement = {
    cite: 0, //String
    dateTime: 0, //String
};
HTMLModElement.prototype = new HTMLElement();

var HTMLObjectElement = {
    align: 0, //String
    archive: 0, //String
    border: 0, //String
    code: 0, //String
    codeBase: 0, //String
    codeType: 0, //String
    contentDocument: 0, //Document
    data: 0, //String
    declare: 0, //Boolean
    form: 0, //HTMLFormElement
    height: 0, //String
    hspace: 0, //Number
    name: 0, //String
    standby: 0, //String
    tabIndex: 0, //Number
    type: 0, //String
    useMap: 0, //String
    vspace: 0, //Number
    width: 0, //String
};
HTMLObjectElement.prototype = new HTMLElement();

var HTMLOListElement = {
    compact: 0, //Boolean
    start: 0, //Number
    type: 0, //String
};
HTMLOListElement.prototype = new HTMLElement();

var HTMLOptGroupElement = {
    disabled: 0, //Boolean
    label: 0, //String
};
HTMLOptGroupElement.prototype = new HTMLElement();

var HTMLOptionElement = {
    defaultSelected: 0, //Boolean
    disabled: 0, //Boolean
    text: 0, //String
    form: 0, //HTMLFormElement
    index: 0, //Number
    label: 0, //String
    selected: 0, //Boolean
    value: 0, //String
};
HTMLOptionElement.prototype = new HTMLElement();

var HTMLParagraphElement = {
    align: 0, //String
};
HTMLParagraphElement.prototype = new HTMLElement();

var HTMLParamElement = {
    name: 0, //String
    type: 0, //String
    value: 0, //String
    valueType: 0, //String
};
HTMLParamElement.prototype = new HTMLElement();

var HTMLPreElement = {
    width: 0, //Number
};
HTMLPreElement.prototype = new HTMLElement();

var HTMLQuoteElement = {
    cite: 0, //String
};
HTMLQuoteElement.prototype = new HTMLElement();

var HTMLScriptElement = {
    charset: 0, //String
    defer: 0, //Boolean
    event: 0, //String
    htmlFor: 0, //String
    src: 0, //String
    text: 0, //String
    type: 0, //String
};
HTMLScriptElement.prototype = new HTMLElement();

var HTMLSelectElement = {
    add: function (element, before) {}, //Object
    blur: function () {}, //Object
    disabled: 0, //Boolean
    focus: function () {}, //Object
    form: 0, //HTMLFormElement
    length: 0, //Number
    multiple: 0, //Boolean
    name: 0, //String
    options: 0, //HTMLOptionsCollection
    remove: function (index) {}, //Object
    selectedIndex: 0, //Number
    size: 0, //Number
    tabIndex: 0, //Number
    type: 0, //String
    value: 0, //String
};
HTMLSelectElement.prototype = new HTMLElement();

var HTMLStyleElement = {
    disabled: 0, //Boolean
    media: 0, //String
    type: 0, //String
    styleSheet: 0, //IE,Stylesheet
};
HTMLStyleElement.prototype = new HTMLElement();

var HTMLTableCaptionElement = {
    align: 0, //String
};
HTMLTableCaptionElement.prototype = new HTMLElement();

var HTMLTableColElement = {
    align: 0, //String
    ch: 0, //String
    chOff: 0, //String
    span: 0, //Number
    vAlign: 0, //String
    width: 0, //String
};
HTMLTableColElement.prototype = new HTMLElement();

var HTMLTableCellElement = {
    abbr: 0, //String
    align: 0, //String
    axis: 0, //String
    bgColor: 0, //String
    cellIndex: 0, //Number
    ch: 0, //String
    chOff: 0, //String
    colSpan: 0, //Number
    headers: 0, //String
    height: 0, //String
    noWrap: 0, //Boolean
    rowSpan: 0, //Number
    scope: 0, //String
    vAlign: 0, //String
    width: 0, //String
};
HTMLTableCellElement.prototype = new HTMLElement();

var HTMLTableElement = {
    align: 0, //String
    bgColor: 0, //String
    border: 0, //String
    caption: 0, //HTMLTableCaptionElement
    cellPadding: 0, //String
    cellSpacing: 0, //String
    createCaption: function () {}, //HTMLElement
    createTFoot: function () {}, //HTMLElement
    createTHead: function () {}, //HTMLElement
    deleteCaption: function () {}, //Object
    deleteRow: function (index) {}, //Object
    deleteTHead: function () {}, //Object
    deleteTFoot: function () {}, //Object
    frame: 0, //String
    insertRow: function (index) {}, //HTMLElement
    rows: 0, //HTMLCollection
    rules: 0, //String
    summary: 0, //String
    tBodies: 0, //HTMLCollection
    tHead: 0, //HTMLTableSectionElement
    tFoot: 0, //HTMLTableSectionElement
    width: 0, //String
};
HTMLTableElement.prototype = new HTMLElement();

var HTMLTableRowElement = {
    align: 0, //String
    bgColor: 0, //String
    cells: 0, //HTMLCollection
    ch: 0, //String
    chOff: 0, //String
    deleteCell: function (index) {}, //Object
    insertCell: function (index) {}, //HTMLElement
    rowIndex: 0, //Number
    sectionRowIndex: 0, //Number
    vAlign: 0, //String
};
HTMLTableRowElement.prototype = new HTMLElement();

var HTMLTableSectionElement = {
    align: 0, //String
    ch: 0, //String
    chOff: 0, //String
    deleteRow: function (index) {}, //Object
    insertRow: function (index) {}, //HTMLElement
    rows: 0, //HTMLCollection
    vAlign: 0, //String
};
HTMLTableSectionElement.prototype = new HTMLElement();

var HTMLTextAreaElement = {
    accessKey: 0, //String
    blur: function () {}, //Object
    cols: 0, //Number
    defaultValue: 0, //String
    disabled: 0, //Boolean
    focus: function () {}, //Object
    form: 0, //HTMLFormElement
    name: 0, //String
    readOnly: 0, //Boolean
    rows: 0, //Number
    select: function () {}, //Object
    tabIndex: 0, //Number
    type: 0, //String
    value: 0, //String
};
HTMLTextAreaElement.prototype = new HTMLElement();

var HTMLTitleElement = {
    text: 0, //String
};
HTMLTitleElement.prototype = new HTMLElement();

var HTMLUListElement = {
    compact: 0, //Boolean
    type: 0, //String
};
HTMLUListElement.prototype = new HTMLElement();

var IEElementStyle = {
    hasLayout: 0, //IE,Boolean
};
IEElementStyle.prototype = new style();
