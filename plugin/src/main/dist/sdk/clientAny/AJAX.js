window.ActiveXObject = function () {}; //XMLHttpRequest
window.XmlHttpRequest = function () {}; //XMLHttpRequest

var XMLHttpRequest = {
    abort: function () {}, //Object
    constructor: 0, //Object
    getAllResponseHeaders: function () {}, //String
    getResponseHeader: function (headerName) {}, //String
    readyState: 0, //Number
    responseText: 0, //String
    responseXML: 0, //String
    onreadystatechange: function () {},
    open: function (method, url, async, username, password) {}, //Object
    overrideMimeType: function (mimeType) {}, //Gecko,Object
    send: function (content) {}, //void
    setRequestHeader: function (label, value) {}, //void
    statusText: 0, //String
    status: 0, //Number
};
XMLHttpRequest.prototype = new Object();
