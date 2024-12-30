window.ActiveXObject = function () {}; //XMLHttpRequest
window.XmlHttpRequest = function () {}; //XMLHttpRequest

var XMLHttpRequest = {
    abort: function () {}, //Object
    constructor: {},
    getAllResponseHeaders: function () {}, //String
    getResponseHeader: function (headerName) {}, //String
    readyState: 0,
    responseText: "",
    responseXML: "",
    onreadystatechange: function () {},
    open: function (method, url, async, username, password) {}, //Object
    overrideMimeType: function (mimeType) {}, //Gecko,Object
    send: function (content) {}, //void
    setRequestHeader: function (label, value) {}, //void
    statusText: "",
    status: 0,
};
XMLHttpRequest.prototype = new Object();
