__defineGetter__ = function (propertyName, func) {}; //Gecko,Object
__defineSetter__ = function (propertyName, func) {}; //Gecko,Object
__lookupGetter__ = function (propertyName) {}; //Gecko,Function
__lookupSetter__ = function (propertyName) {}; //Gecko,Function
__proto__ = function (propertyName) {}; //Gecko,Function

var Image = function () {};
Image.prototype = new HTMLImageElement();
Image.constructor = {};
Image.naturalWidth = {}; //Gecko
Image.naturalHeight = {}; //Gecko

var Navigator = {
    appCodeName: "",
    appName: "",
    appVersion: "",
    javaEnabled: function () {}, //Boolean
    language: "",
    mimeTypes: [],
    platform: [],
    plugins: "",
    preference: function (prefName, prefValue) {}, //String
    product: "",
    systemLanguage: "", //IE
    taintEnabled: function () {}, //Boolean
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
    reload: function () {}, //void
    replace: function (url) {}, //void
    search: "",
    target: "",
};

var XSLTProcessor = function () {};
XSLTProcessor.prototype = new Object();
XSLTProcessor.constructor = {}; //Gecko
XSLTProcessor.importStylesheet = function (domNode) {}; //Gecko,Object
XSLTProcessor.transformToDocument = function (domNode) {}; //Gecko,Document
XSLTProcessor.transformToFragment = function (domToBeTransformed, ownerDocument) {}; //Gecko,DocumentFragment

var DOMParser = function () {};
DOMParser.prototype = new Object();
DOMParser.constructor = {}; //Gecko
DOMParser.parseFromString = function (stringToParse, contentType) {}; //Gecko,Document

var XMLSerializer = function () {};
XMLSerializer.prototype = new Object();
XMLSerializer.constructor = 0; //Gecko,Object
XMLSerializer.serializeToStream = function (domNode, stream, encoding) {}; //Gecko,Object
XMLSerializer.serializeToString = function (domNode) {}; //Gecko,String

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
    collapse: function (parentNode, offset) {}, //Gecko,void
    collapseToStart: function () {}, //Gecko,void
    collapseToEnd: function () {}, //Gecko,void
    containsNode: function (node, partlyContained) {}, //Gecko,Boolean
    createRange: function () {}, //IE,TextRange
    empty: function () {}, //IE,Boolean
    extend: function (parentNode, offset) {}, //Gecko,void
    focusNode: {}, //Gecko,Node
    focusOffset: 0, //Gecko
    getRangeAt: function (index) {}, //Gecko,Range
    isCollapsed: false, //Gecko
    rangeCount: 0, //Gecko
    removeRange: function (range) {}, //Gecko,void
    removeAllRanges: function () {}, //Gecko,void
    selectAllChildren: function (parentNode) {}, //Gecko,void
    type: {}, //IE
};

var TextRange = {
    boundingHeight: 0, //IE
    boundingLeft: 0, //IE
    boundingTop: 0, //IE
    boundingWidth: 0, //IE
    collapse: function (start) {}, //IE,void
    compareEndPoint: function (type, range) {}, //IE,void
    compareEndPoints: function (sType, oRange) {}, //IE,Number
    duplicate: function () {}, //IE,TextRange
    expand: function (unit) {}, //IE,void
    htmlText: "", //IE
    inRange: function (other) {}, //IE,boolean
    isEqual: function (other) {}, //IE,boolean
    move: function (unit, count) {}, //IE,void
    moveEnd: function (unit, count) {}, //IE,void
    moveStart: function (unit, count) {}, //IE,void
    moveToElementText: function (element) {}, //IE,void
    offsetLeft: 0, //IE
    offsetTop: 0, //IE
    parentElement: function () {}, //IE,Node
    pasteHTML: function (htmlText) {}, //IE,void
    scrollIntoView: function (start) {}, //IE,void
    select: function () {}, //IE,void
    setEndPoint: function (type, range) {}, //IE,void
    text: "", //IE
};

var document = {
    all: 0, //IE,All
    execCommand: function (sCommand, bUserInterface, vValue) {}, //IE,Boolean
    namespaces: [], //IE
    selection: {}, //IE,Selection
    styleSheets: [], //Stylesheet[]
};

var CssRule = {
    selectorText: "",
};

var Stylesheet = {
    addRule: function (selector, style) {}, //IE,void
    cssRules: [], //Gecko,CssRule[]
    deleteRule: function (index) {}, //Gecko,void
    insertRule: function (ruleText, index) {}, //Gecko,void
    owningElement: {}, //IE,HtmlElement
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
    defaultStatus: "",
    detachEvent: function (type, listener) {}, //IE,Object
    directories: {},
    document: {}, //HTMLDocument
    external: {}, //IE,IEExternal
    event: {}, //Event
    execScript: function (sScript) {}, //IE,Object
    frameElement: {},
    frames: [],
    getComputedStyle: function (element, pseudoElt) {}, //Gecko,style
    GetObject: function (classOrPath, clazz) {}, //IE,Object,deprecated
    getSelection: function () {}, //Gecko,Selection
    history: {}, //history
    innerHeight: 0,
    innerWidth: 0,
    location: {}, //Location
    menubar: {},
    moveBy: function (xDelta, yDelta) {}, //Object
    moveTo: function (x, y) {}, //Object
    navigator: {}, //Navigator
    opener: {}, //window
    opera: false, //Opera
    outerHeight: 0,
    outerWidth: 0,
    pageXOffset: 0,
    pageYOffset: 0,
    parent: {}, //window
    preventBubble: function (event) {}, //Gecko,void,deprecated
    print: function () {}, //void
    prompt: function (sMessage, sDefaultValue) {}, //Object
    releaseEvents: function (eventType) {}, //Gecko,void,deprecated
    removeEventListener: function (type, listener, useCapture) {}, //Gecko,Object
    resizeBy: function (widthDelta, heightDelta) {}, //Object
    resizeTo: function (width, height) {}, //Object
    routeEvent: function (event) {}, //Gecko,void,deprecated
    screen: {}, //Screen
    screenLeft: 0, //IE
    screenTop: 0, //IE
    ScriptEngine: function () {}, //IE,String
    ScriptEngineBuildVersion: function () {}, //IE,String
    ScriptEngineMajorVersion: function () {}, //IE,String
    ScriptEngineMinorVersion: function () {}, //IE,String
    scrollbars: {},
    scrollBy: function (xDelta, yDelta) {}, //Object
    scrollMaxX: 0,
    scrollMaxY: 0,
    scrollTo: function (x, y) {}, //Object
    scrollX: 0,
    scrollY: 0,
    self: {},
    setInterval: function (vCode, iMillis) {}, //Number
    setTimeout: function (vCode, iMillis) {}, //Number
    showModalDialog: function (sUrl, vArguments, sFeatures) {}, //IE,style
    sidebar: {}, //Gecko,MozillaSideBar
    status: "",
    toolbar: {},
    top: 0, //window
    window: 0, //window
};

var escape = function () {}; //Object,deprecated
var unescape = function () {}; //Object,deprecated

var HTMLCollection = {
    length: 0,
    item: function (index) {}, //Node
    namedItem: function (name) {}, //Node
};

var HTMLOptionsCollection = {
    item: function (index) {}, //Node
    length: 0,
    namedItem: function (name) {}, //Node
};

var HTMLDocument = {
    activeElement: {}, //IE,DocumentView
    anchors: {}, //HTMLCollection
    applets: {}, //HTMLCollection
    body: {}, //HTMLElement
    close: function () {}, //Object
    commandDispatcher: {}, //Gecko,CommandDispatcher
    compatMode: "",
    cookie: {}, //HTMLCollection
    createEventObject: function (oExistingEvent) {}, //IE,Even
    createStyleSheet: function () {}, //IE,Stylesheet
    defaultView: {}, //Gecko,DocumentView
    domain: "",
    elementFromPoint: function (iX, iY) {}, //IE,HTMLElement
    forms: {}, //HTMLCollection
    getAnonymousElementByAttribute: function (node, attrName, attrValue) {}, //Gecko,NodeList
    getAnonymousNodes: function (node) {}, //Gecko,Node[]
    getBoxObjectFor: function (element) {}, //Gecko,Node
    getElementsByName: function (elementName) {}, //NodeList
    getElementsByClassName: function (className) {}, //Gecko,NodeList
    images: {}, //HTMLCollection
    links: {}, //HTMLCollection
    open: function () {}, //Object
    parentWindow: {}, //IE,window
    querySelector: function (string) {}, //Gecko,Node
    querySelectorAll: function (string) {}, //Gecko,NodeList
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
    getComputedStyle: function (element, type) {}, //Gecko,CssStyle
};

var CssStyle = {
    getPropertyValue: function (propertyName) {}, //Gecko,String
};

var HTMLElement = {
    addBehavior: function (sUrl) {}, //IE,Number
    attachEvent: function (type, listener) {}, //IE,Object
    boxObject: {}, //Gecko,HTMLElement
    children: [], //IE,HTMLElement[]
    className: "",
    clearAttributes: function () {}, //IE,Object
    clientHeight: 0,
    clientLeft: 0,
    clientTop: 0,
    clientWidth: 0,
    currentStyle: {}, //IE,IEElementStyle
    detachEvent: function (type, listener) {}, //IE,Object
    dir: "",
    filters: [], //IE
    fireEvent: function (type, event) {}, //IE,Object
    getBoundingClientRect: function () {}, //IE,TextRange
    hidePopup: function () {}, //Gecko,Object
    id: "",
    innerHTML: "",
    innerText: "", //IE
    insertAdjacentHTML: function (position, htmlContent) {}, //IE,Object
    insertAdjacentText: function (position, textContent) {}, //IE,Object
    isDisabled: false, //IE
    lang: "",
    mergeAttributes: function (oSource, bPreserve) {}, //IE,Object
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
    releaseCapture: function () {}, //IE,Object
    removeBehavior: function (sID) {}, //IE,Boolean
    runtimeStyle: {}, //IE,IEElementStyle
    setCapture: function (bContainerCapture) {}, //IE,Object
    scrollWidth: 0,
    scrollHeight: 0,
    scrollTop: 0,
    scrollLeft: 0,
    showPopup: function () {}, //Gecko,Object
    style: 0, //style
    stylesheet: 0, //Stylesheet
    title: "",
};
HTMLElement.prototype = new Element();

var HTMLAnchorElement = {
    accessKey: "",
    charset: "",
    coords: "",
    blur: function () {}, //Object
    focus: function () {}, //Object
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
    reset: function () {}, //Object
    submit: function () {}, //Object
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
    blur: function () {}, //Object
    checked: false,
    click: function () {}, //Object
    createTextRange: function () {}, //IE,TextRange
    defaultChecked: false,
    defaultValue: "",
    disabled: false,
    focus: function () {}, //Object
    form: {}, //HTMLFormElement
    maxLength: 0,
    name: "",
    readOnly: false,
    select: function () {}, //Object
    setSelectionRange: function (start, end) {}, //Gecko,Object
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
    add: function (element, before) {}, //Object
    blur: function () {}, //Object
    disabled: false,
    focus: function () {}, //Object
    form: {}, //HTMLFormElement
    length: 0,
    multiple: false,
    name: "",
    options: {}, //HTMLOptionsCollection
    remove: function (index) {}, //Object
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
    createCaption: function () {}, //HTMLElement
    createTFoot: function () {}, //HTMLElement
    createTHead: function () {}, //HTMLElement
    deleteCaption: function () {}, //Object
    deleteRow: function (index) {}, //Object
    deleteTHead: function () {}, //Object
    deleteTFoot: function () {}, //Object
    frame: "",
    insertRow: function (index) {}, //HTMLElement
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
    deleteCell: function (index) {}, //Object
    insertCell: function (index) {}, //HTMLElement
    rowIndex: 0,
    sectionRowIndex: 0,
    vAlign: "",
};
HTMLTableRowElement.prototype = new HTMLElement();

var HTMLTableSectionElement = {
    align: "",
    ch: "",
    chOff: "",
    deleteRow: function (index) {}, //Object
    insertRow: function (index) {}, //HTMLElement
    rows: {}, //HTMLCollection
    vAlign: "",
};
HTMLTableSectionElement.prototype = new HTMLElement();

var HTMLTextAreaElement = {
    accessKey: "",
    blur: function () {}, //Object
    cols: 0,
    defaultValue: "",
    disabled: false,
    focus: function () {}, //Object
    form: {}, //HTMLFormElement
    name: "",
    readOnly: false,
    rows: 0,
    select: function () {}, //Object
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
