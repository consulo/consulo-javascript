__defineGetter__ = function (propertyName, func) { return {}; }; //Gecko
__defineSetter__ = function (propertyName, func) { return {}; }; //Gecko
__lookupGetter__ = function (propertyName) { return {}; }; //Gecko,Function
__lookupSetter__ = function (propertyName) { return {}; }; //Gecko,Function
__proto__ = function (propertyName) { return {}; }; //Gecko,Function

var Image = function () {};
Image.prototype = new HTMLImageElement();
Image.constructor = {};
Image.naturalWidth = {}; //Gecko
Image.naturalHeight = {}; //Gecko

var Navigator = {
    appCodeName: "",
    appName: "",
    appVersion: "",
    javaEnabled: function () { return false; },
    language: "",
    mimeTypes: [],
    platform: [],
    plugins: "",
    preference: function (prefName, prefValue) { return ""; },
    product: "",
    systemLanguage: "", //IE
    taintEnabled: function () { return false; },
    userAgent: "",
    userLanguage: "", //IE
};

var Screen = {
    availHeight: 0,
    availWidth: 0,
    colorDepth: 0,
    height: 0,
    pixelDepth: 0,
    width: 0,
};

var location = {
    href: "",
    hash: "",
    host: "",
    hostname: "",
    pathname: "",
    port: "",
    protocol: "",
    reload: function () {},
    replace: function (url) {},
    search: "",
    target: "",
};

var XSLTProcessor = function () {};
XSLTProcessor.prototype = new Object();
XSLTProcessor.constructor = {}; //Gecko
XSLTProcessor.importStylesheet = function (domNode) { return {}; }; //Gecko
XSLTProcessor.transformToDocument = function (domNode) { return {}; }; //Gecko,Document
XSLTProcessor.transformToFragment = function (domToBeTransformed, ownerDocument) { return {}; }; //Gecko,DocumentFragment

var DOMParser = function () {};
DOMParser.prototype = new Object();
DOMParser.constructor = {}; //Gecko
DOMParser.parseFromString = function (stringToParse, contentType) { return {}; }; //Gecko,Document

var XMLSerializer = function () {};
XMLSerializer.prototype = new Object();
XMLSerializer.constructor = 0; //Gecko,Object
XMLSerializer.serializeToStream = function (domNode, stream, encoding) { return {}; }; //Gecko
XMLSerializer.serializeToString = function (domNode) { return ""; }; //Gecko

var Event = {
    altKey: false,
    button: false,
    cancelBubble: false, //IE
    charCode: "", //Gecko
    clientX: 0,
    clientY: 0,
    ctrlKey: false,
    data: {},
    height: 0,
    fromElement: {}, //IE
    keyCode: 0,
    layerX: 0, //Gecko
    layerY: 0, //Gecko
    modifiers: 0,
    offsetX: 0, //IE
    offsetY: 0, //IE
    pageX: 0,
    pageY: 0,
    repeat: false, //IE
    returnValue: false, //IE
    screenX: 0,
    screenY: 0,
    shiftKey: false,
    srcElement: {}, //IE
    toElement: {}, //IE
    type: "",
    wheelDelta: 0, //IE
    which: {},
    width: 0,
    x: 0, //IE
    y: 0, //IE
    // deprecated
    ABORT: "", //Gecko,deprecated
    BLUR: "", //Gecko,deprecated
    CLICK: "", //Gecko,deprecated
    CHANGE: "", //Gecko,deprecated
    DBLCLICK: "", //Gecko,deprecated
    DRAGDROP: "", //Gecko,deprecated
    ERROR: "", //Gecko,deprecated
    FOCUS: "", //Gecko,deprecated
    KEYDOWN: "", //Gecko,deprecated
    KEYPRESS: "", //Gecko,deprecated
    KEYUP: "", //Gecko,deprecated
    LOAD: "", //Gecko,deprecated
    MOUSEDOWN: "", //Gecko,deprecated
    MOUSEMOVE: "", //Gecko,deprecated
    MOUSEOUT: "", //Gecko,deprecated
    MOUSEOVER: "", //Gecko,deprecated
    MOUSEUP: "", //Gecko,deprecated
    MOVE: "", //Gecko,deprecated
    RESET: "", //Gecko,deprecated
    RESIZE: "", //Gecko,deprecated
    SELECT: "", //Gecko,deprecated
    SUBMIT: "", //Gecko,deprecated
    UNLOAD: "", //Gecko,deprecated
};

var Selection = {
    addRange: function (range) {}, //Gecko,void
    anchorNode: {}, //Gecko,Node
    anchorOffset: 0, //Gecko
    clear: function () {}, //IE,Boolean
    collapse: function (parentNode, offset) {}, //Gecko
    collapseToStart: function () {}, //Gecko
    collapseToEnd: function () {}, //Gecko
    containsNode: function (node, partlyContained) { return false; }, //Gecko
    createRange: function () { return {}; }, //IE,TextRange
    empty: function () { return true; }, //IE
    extend: function (parentNode, offset) {}, //Gecko
    focusNode: {}, //Gecko,Node
    focusOffset: 0, //Gecko
    getRangeAt: function (index) { return {}; }, //Gecko,Range
    isCollapsed: false, //Gecko
    rangeCount: 0, //Gecko
    removeRange: function (range) {}, //Gecko
    removeAllRanges: function () {}, //Gecko
    selectAllChildren: function (parentNode) {}, //Gecko
    type: {}, //IE
};

var TextRange = {
    boundingHeight: 0, //IE
    boundingLeft: 0, //IE
    boundingTop: 0, //IE
    boundingWidth: 0, //IE
    collapse: function (start) {}, //IE,void
    compareEndPoint: function (type, range) {}, //IE
    compareEndPoints: function (sType, oRange) { return 0; }, //IE
    duplicate: function () { return {}; }, //IE,TextRange
    expand: function (unit) {}, //IE
    htmlText: "", //IE
    inRange: function (other) { return false; }, //IE
    isEqual: function (other) { return false; }, //IE
    move: function (unit, count) {}, //IE
    moveEnd: function (unit, count) {}, //IE
    moveStart: function (unit, count) {}, //IE
    moveToElementText: function (element) {}, //IE
    offsetLeft: 0, //IE
    offsetTop: 0, //IE
    parentElement: function () { return {}; }, //IE,Node
    pasteHTML: function (htmlText) {}, //IE
    scrollIntoView: function (start) {}, //IE
    select: function () {}, //IE
    setEndPoint: function (type, range) {}, //IE
    text: "", //IE
};

var document = {
    all: 0, //IE,All
    execCommand: function (sCommand, bUserInterface, vValue) { return false; }, //IE
    namespaces: [], //IE
    selection: {}, //IE,Selection
    styleSheets: [], //Stylesheet[]
};

var CssRule = {
    selectorText: "",
};

var Stylesheet = {
    addRule: function (selector, style) {}, //IE
    cssRules: [], //Gecko,CssRule[]
    deleteRule: function (index) {}, //Gecko
    insertRule: function (ruleText, index) {}, //Gecko
    owningElement: {}, //IE,HtmlElement
    removeRule: function (index) {}, //IE
    rules: 0, //IE,CssRule[]
};

var MozillaSideBar = {
    addPanel: function (title, url, param3) {}, //Gecko
};

var IEExternal = {
    AddFavorite: function (title, url) {}, //IE
};

var history = {
    back: function () {},
    forward: function () {},
    go: function (count) {},
};

var window = {
    addEventListener: function (type, listener, useCapture) { return {}; }, //Gecko
    alert: function (sMesssage) { return {}; },
    attachEvent: function (type, listener) { return {}; }, //IE
    captureEvents: function (eventType) {}, //Gecko,deprecated
    clearInterval: function (intervalId) { return {}; },
    clearTimeout: function (intervalId) { return {}; },
    CollectGarbage: function () { return {}; }, //IE
    confirm: function (sMesssage) { return {}; },
    defaultStatus: "",
    detachEvent: function (type, listener) { return {}; }, //IE
    directories: {},
    document: {}, //HTMLDocument
    external: {}, //IE,IEExternal
    event: {}, //Event
    execScript: function (sScript) { return {}; }, //IE
    frameElement: {},
    frames: [],
    getComputedStyle: function (element, pseudoElt) { return {}; }, //Gecko,style
    GetObject: function (classOrPath, clazz) { return {}; }, //IE,deprecated
    getSelection: function () { return {}; }, //Gecko,Selection
    history: {}, //history
    innerHeight: 0,
    innerWidth: 0,
    location: {}, //Location
    menubar: {},
    moveBy: function (xDelta, yDelta) { return {}; },
    moveTo: function (x, y) { return {}; },
    navigator: {}, //Navigator
    opener: {}, //window
    opera: false, //Opera
    outerHeight: 0,
    outerWidth: 0,
    pageXOffset: 0,
    pageYOffset: 0,
    parent: {}, //window
    preventBubble: function (event) {}, //Gecko,deprecated
    print: function () {},
    prompt: function (sMessage, sDefaultValue) { return {}; },
    releaseEvents: function (eventType) {}, //Gecko,deprecated
    removeEventListener: function (type, listener, useCapture) { return {}; }, //Gecko
    resizeBy: function (widthDelta, heightDelta) { return {}; },
    resizeTo: function (width, height) { return {}; },
    routeEvent: function (event) {}, //Gecko,deprecated
    screen: {}, //Screen
    screenLeft: 0, //IE
    screenTop: 0, //IE
    ScriptEngine: function () { return ""; }, //IE
    ScriptEngineBuildVersion: function () { return ""; }, //IE
    ScriptEngineMajorVersion: function () { return ""; }, //IE
    ScriptEngineMinorVersion: function () { return ""; }, //IE
    scrollbars: {},
    scrollBy: function (xDelta, yDelta) { return {}; },
    scrollMaxX: 0,
    scrollMaxY: 0,
    scrollTo: function (x, y) { return {}; },
    scrollX: 0,
    scrollY: 0,
    self: {},
    setInterval: function (vCode, iMillis) { return 0; },
    setTimeout: function (vCode, iMillis) { return 0; },
    showModalDialog: function (sUrl, vArguments, sFeatures) { return {}; }, //IE,style
    sidebar: {}, //Gecko,MozillaSideBar
    status: "",
    toolbar: {},
    top: {}, //window
    window: {}, //window
};

var escape = function () { return {}; }; //deprecated
var unescape = function () { return {}; }; //deprecated

var HTMLCollection = {
    length: 0,
    item: function (index) { return {}; }, //Node
    namedItem: function (name) { return {}; }, //Node
};

var HTMLOptionsCollection = {
    item: function (index) { return {}; }, //Node
    length: 0,
    namedItem: function (name) { return {}; }, //Node
};

var HTMLDocument = {
    activeElement: {}, //IE,DocumentView
    anchors: {}, //HTMLCollection
    applets: {}, //HTMLCollection
    body: {}, //HTMLElement
    close: function () { return {}; },
    commandDispatcher: {}, //Gecko,CommandDispatcher
    compatMode: "",
    cookie: {}, //HTMLCollection
    createEventObject: function (oExistingEvent) { return {}; }, //IE,Event
    createStyleSheet: function () { return {}; }, //IE,Stylesheet
    defaultView: {}, //Gecko,DocumentView
    domain: "",
    elementFromPoint: function (iX, iY) { return {}; }, //IE,HTMLElement
    forms: {}, //HTMLCollection
    getAnonymousElementByAttribute: function (node, attrName, attrValue) { return {}; }, //Gecko,NodeList
    getAnonymousNodes: function (node) { return []; }, //Gecko,Node[]
    getBoxObjectFor: function (element) { return {}; }, //Gecko,Node
    getElementsByName: function (elementName) { return {}; }, //NodeList
    getElementsByClassName: function (className) { return {}; }, //Gecko,NodeList
    images: {}, //HTMLCollection
    links: {}, //HTMLCollection
    open: function () { return {}; },
    parentWindow: {}, //IE,window
    querySelector: function (string) { return {}; }, //Gecko,Node
    querySelectorAll: function (string) { return {}; }, //Gecko,NodeList
    referrer: "",
    title: "",
    URL: "",
    write: function (text) {},
    writeln: function (text) {},
};
HTMLDocument.prototype = new Document();

var CommandDispatcher = {
    focusedElement: {}, //Gecko,HTMLElement
};

var DocumentView = {
    getComputedStyle: function (element, type) { return {}; }, //Gecko,CssStyle
};

var CssStyle = {
    getPropertyValue: function (propertyName) { return {}; }, //Gecko,String
};

var HTMLElement = {
    addBehavior: function (sUrl) { return 0; }, //IE
    attachEvent: function (type, listener) { return {}; }, //IE
    boxObject: {}, //Gecko,HTMLElement
    children: [], //IE,HTMLElement[]
    className: "",
    clearAttributes: function () { return {}; }, //IE
    clientHeight: 0,
    clientLeft: 0,
    clientTop: 0,
    clientWidth: 0,
    currentStyle: {}, //IE,IEElementStyle
    detachEvent: function (type, listener) { return {}; }, //IE
    dir: "",
    filters: [], //IE
    fireEvent: function (type, event) { return {}; }, //IE
    getBoundingClientRect: function () {}, //IE,TextRange
    hidePopup: function () {}, //Gecko,Object
    id: "",
    innerHTML: "",
    innerText: "", //IE
    insertAdjacentHTML: function (position, htmlContent) { return {}; }, //IE
    insertAdjacentText: function (position, textContent) { return {}; }, //IE
    isDisabled: false, //IE
    lang: "",
    mergeAttributes: function (oSource, bPreserve) { return {}; }, //IE
    offsetHeight: 0,
    offsetLeft: 0,
    offsetParent: 0,
    offsetTop: 0,
    offsetWidth: 0,
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
    propertyName: "", //IE
    releaseCapture: function () { return {}; }, //IE
    removeBehavior: function (sID) { return false; }, //IE
    runtimeStyle: {}, //IE,IEElementStyle
    setCapture: function (bContainerCapture) { return {}; }, //IE
    scrollWidth: 0,
    scrollHeight: 0,
    scrollTop: 0,
    scrollLeft: 0,
    showPopup: function () { return {}; }, //Gecko
    style: {}, //style
    stylesheet: {}, //Stylesheet
    title: "",
};
HTMLElement.prototype = new Element();

var HTMLAnchorElement = {
    accessKey: "",
    charset: "",
    coords: "",
    blur: function () { return {}; },
    focus: function () { return {}; },
    href: "",
    hreflang: "",
    name: "",
    rel: "",
    rev: "",
    shape: "",
    tabIndex: 0,
    target: "",
    type: "",
};
HTMLAnchorElement.prototype = new HTMLElement();

var HTMLAppletElement = {
    align: "",
    alt: "",
    archive: "",
    code: "",
    codeBase: "",
    height: "",
    hspace: 0,
    name: "",
    object: "",
    vspace: 0,
    width: "",
};
HTMLAppletElement.prototype = new HTMLElement();

var HTMLAreaElement = {
    accessKey: "",
    alt: "",
    coords: "",
    href: "",
    noHref: false,
    shape: "",
    tabIndex: 0,
    target: "",
};
HTMLAreaElement.prototype = new HTMLElement();

var HTMLBaseElement = {
    href: "",
    target: "",
};
HTMLBaseElement.prototype = new HTMLElement();

var HTMLBaseFontElement = {
    color: "",
    face: "",
    size: 0,
};
HTMLBaseFontElement.prototype = new HTMLElement();

var HTMLBodyElement = {
    aLink: "",
    background: "",
    bgColor: "",
    link: "",
    onload: function () {},
    onunload: function () {},
    text: "",
    vLink: "",
};
HTMLBodyElement.prototype = new HTMLElement();

var HTMLBRElement = {
    clear: "",
};
HTMLBRElement.prototype = new HTMLElement();

var HTMLButtonElement = {
    accessKey: "",
    disabled: false,
    form: {}, //HTMLFormElement
    name: "",
    tabIndex: 0,
    type: "",
    value: "",
};
HTMLButtonElement.prototype = new HTMLElement();

var HTMLDirectoryElement = {
    compact: false,
};
HTMLDirectoryElement.prototype = new HTMLElement();

var HTMLDivElement = {
    align: "",
};
HTMLDivElement.prototype = new HTMLElement();

var HTMLDListElement = {
    compact: false,
};
HTMLDListElement.prototype = new HTMLElement();

var HTMLFieldSetElement = {
    form: {}, //HTMLFormElement
};
HTMLFieldSetElement.prototype = new HTMLElement();

var HTMLFontElement = {
    color: "",
    face: "",
    size: "",
};
HTMLFontElement.prototype = new HTMLElement();

var HTMLFormElement = {
    acceptCharset: "",
    action: "",
    elements: {}, //HTMLCollection
    enctype: "",
    length: 0,
    method: "",
    name: "",
    reset: function () { return {}; },
    submit: function () { return {}; },
    target: "",
};

var HTMLFrameElement = {
    contentDocument: {}, //Gecko,Document
    frameBorder: "",
    longDesc: "",
    marginHeight: "",
    marginWidth: "",
    name: "",
    noResize: false,
    scrolling: "",
    src: "",
};
HTMLFrameElement.prototype = new HTMLElement();

var HTMLFrameSetElement = {
    cols: "",
    rows: "",
};
HTMLFrameSetElement.prototype = new HTMLElement();

var HTMLHeadElement = {
    profile: "",
};
HTMLHeadElement.prototype = new HTMLElement();

var HTMLHeadingElement = {
    align: "",
};
HTMLHeadingElement.prototype = new HTMLElement();

var HTMLHtmlElement = {
    version: "",
};
HTMLHtmlElement.prototype = new HTMLElement();

var HTMLInputElement = {
    accept: "",
    accessKey: "",
    align: "",
    alt: "",
    blur: function () { return {}; },
    checked: false,
    click: function () { return {}; },
    createTextRange: function () { return {}; }, //IE,TextRange
    defaultChecked: false,
    defaultValue: "",
    disabled: false,
    focus: function () { return {}; },
    form: {}, //HTMLFormElement
    maxLength: 0,
    name: "",
    readOnly: false,
    select: function () { return {}; },
    setSelectionRange: function (start, end) { return {}; }, //Gecko
    size: 0,
    src: "",
    tabIndex: 0,
    type: "",
    useMap: "",
    value: "",
};
HTMLInputElement.prototype = new HTMLElement();

var HTMLIsIndexElement = {
    form: {}, //HTMLFormElement
    prompt: "",
};
HTMLIsIndexElement.prototype = new HTMLElement();

var HTMLHRElement = {
    align: "",
    noShade: false,
    size: "",
    width: "",
};
HTMLHRElement.prototype = new HTMLElement();

var HTMLIFrameElement = {
    align: "",
    contentDocument: {}, //Document
    contentWindow: {}, //window
    frameBorder: "",
    height: "",
    longDesc: "",
    marginHeight: "",
    marginWidth: "",
    name: "",
    scrolling: "",
    src: "",
    width: "",
};
HTMLIFrameElement.prototype = new HTMLElement();

var HTMLImageElement = {
    align: "",
    alt: "",
    border: "",
    height: 0,
    hspace: 0,
    isMap: false,
    longDesc: "",
    name: "",
    src: "",
    useMap: "",
    vspace: 0,
    width: 0,
};
HTMLImageElement.prototype = new HTMLElement();

var HTMLLabelElement = {
    accessKey: "",
    form: {}, //HTMLFormElement
    htmlFor: 0,
};
HTMLLabelElement.prototype = new HTMLElement();

var HTMLLegendElement = {
    accessKey: "",
    align: "",
    form: {}, //HTMLFormElement
};
HTMLLegendElement.prototype = new HTMLElement();

var HTMLLIElement = {
    type: "",
    value: 0,
};
HTMLLIElement.prototype = new HTMLElement();

var HTMLLinkElement = {
    charset: "",
    disabled: false,
    href: "",
    hreflang: "",
    media: "",
    rel: "",
    rev: "",
    target: "",
    type: "",
};
HTMLLinkElement.prototype = new HTMLElement();

var HTMLMapElement = {
    areas: {}, //HTMLCollection
    name: "",
};
HTMLMapElement.prototype = new HTMLElement();

var HTMLMenuElement = {
    compact: false,
};
HTMLMenuElement.prototype = new HTMLElement();

var HTMLMetaElement = {
    content: "",
    httpEquiv: "",
    name: "",
    scheme: "",
};
HTMLMetaElement.prototype = new HTMLElement();

var HTMLModElement = {
    cite: "",
    dateTime: "",
};
HTMLModElement.prototype = new HTMLElement();

var HTMLObjectElement = {
    align: "",
    archive: "",
    border: "",
    code: "",
    codeBase: "",
    codeType: "",
    contentDocument: {}, //Document
    data: "",
    declare: false,
    form: {}, //HTMLFormElement
    height: "",
    hspace: 0,
    name: "",
    standby: "",
    tabIndex: 0,
    type: "",
    useMap: "",
    vspace: 0,
    width: "",
};
HTMLObjectElement.prototype = new HTMLElement();

var HTMLOListElement = {
    compact: false,
    start: 0,
    type: "",
};
HTMLOListElement.prototype = new HTMLElement();

var HTMLOptGroupElement = {
    disabled: false,
    label: "",
};
HTMLOptGroupElement.prototype = new HTMLElement();

var HTMLOptionElement = {
    defaultSelected: false,
    disabled: false,
    text: "",
    form: {}, //HTMLFormElement
    index: 0,
    label: "",
    selected: false,
    value: "",
};
HTMLOptionElement.prototype = new HTMLElement();

var HTMLParagraphElement = {
    align: "",
};
HTMLParagraphElement.prototype = new HTMLElement();

var HTMLParamElement = {
    name: "",
    type: "",
    value: "",
    valueType: "",
};
HTMLParamElement.prototype = new HTMLElement();

var HTMLPreElement = {
    width: 0,
};
HTMLPreElement.prototype = new HTMLElement();

var HTMLQuoteElement = {
    cite: "",
};
HTMLQuoteElement.prototype = new HTMLElement();

var HTMLScriptElement = {
    charset: "",
    defer: false,
    event: "",
    htmlFor: "",
    src: "",
    text: "",
    type: "",
};
HTMLScriptElement.prototype = new HTMLElement();

var HTMLSelectElement = {
    add: function (element, before) { return {}; },
    blur: function () { return {}; },
    disabled: false,
    focus: function () { return {}; },
    form: {}, //HTMLFormElement
    length: 0,
    multiple: false,
    name: "",
    options: {}, //HTMLOptionsCollection
    remove: function (index) { return {}; },
    selectedIndex: 0,
    size: 0,
    tabIndex: 0,
    type: "",
    value: "",
};
HTMLSelectElement.prototype = new HTMLElement();

var HTMLStyleElement = {
    disabled: false,
    media: "",
    type: "",
    styleSheet: {}, //IE,Stylesheet
};
HTMLStyleElement.prototype = new HTMLElement();

var HTMLTableCaptionElement = {
    align: "",
};
HTMLTableCaptionElement.prototype = new HTMLElement();

var HTMLTableColElement = {
    align: "",
    ch: "",
    chOff: "",
    span: 0,
    vAlign: "",
    width: "",
};
HTMLTableColElement.prototype = new HTMLElement();

var HTMLTableCellElement = {
    abbr: "",
    align: "",
    axis: "",
    bgColor: "",
    cellIndex: 0,
    ch: "",
    chOff: "",
    colSpan: 0,
    headers: "",
    height: "",
    noWrap: false,
    rowSpan: 0,
    scope: "",
    vAlign: "",
    width: "",
};
HTMLTableCellElement.prototype = new HTMLElement();

var HTMLTableElement = {
    align: "",
    bgColor: "",
    border: "",
    caption: {}, //HTMLTableCaptionElement
    cellPadding: "",
    cellSpacing: "",
    createCaption: function () { return {}; }, //HTMLElement
    createTFoot: function () { return {}; }, //HTMLElement
    createTHead: function () { return {}; }, //HTMLElement
    deleteCaption: function () { return {}; },
    deleteRow: function (index) { return {}; },
    deleteTHead: function () { return {}; },
    deleteTFoot: function () { return {}; },
    frame: "",
    insertRow: function (index) { return {}; }, //HTMLElement
    rows: {}, //HTMLCollection
    rules: "",
    summary: "",
    tBodies: {}, //HTMLCollection
    tHead: {}, //HTMLTableSectionElement
    tFoot: {}, //HTMLTableSectionElement
    width: "",
};
HTMLTableElement.prototype = new HTMLElement();

var HTMLTableRowElement = {
    align: "",
    bgColor: "",
    cells: {}, //HTMLCollection
    ch: "",
    chOff: "",
    deleteCell: function (index) { return {}; },
    insertCell: function (index) { return {}; }, //HTMLElement
    rowIndex: 0,
    sectionRowIndex: 0,
    vAlign: "",
};
HTMLTableRowElement.prototype = new HTMLElement();

var HTMLTableSectionElement = {
    align: "",
    ch: "",
    chOff: "",
    deleteRow: function (index) { return {}; },
    insertRow: function (index) { return {}; }, //HTMLElement
    rows: {}, //HTMLCollection
    vAlign: "",
};
HTMLTableSectionElement.prototype = new HTMLElement();

var HTMLTextAreaElement = {
    accessKey: "",
    blur: function () { return {}; },
    cols: 0,
    defaultValue: "",
    disabled: false,
    focus: function () { return {}; },
    form: {}, //HTMLFormElement
    name: "",
    readOnly: false,
    rows: 0,
    select: function () { return {}; },
    tabIndex: 0,
    type: "",
    value: "",
};
HTMLTextAreaElement.prototype = new HTMLElement();

var HTMLTitleElement = {
    text: "",
};
HTMLTitleElement.prototype = new HTMLElement();

var HTMLUListElement = {
    compact: false,
    type: "",
};
HTMLUListElement.prototype = new HTMLElement();

var IEElementStyle = {
    hasLayout: false, //IE
};
IEElementStyle.prototype = new style();
