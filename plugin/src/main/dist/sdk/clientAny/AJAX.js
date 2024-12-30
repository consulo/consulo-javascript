window.ActiveXObject = function () { return {}; }; //XMLHttpRequest
window.XmlHttpRequest = function () { return {}; }; //XMLHttpRequest

var XMLHttpRequest = {
    abort: function () {}, //Object
    constructor: {},
    getAllResponseHeaders: function () { return ""; },
    getResponseHeader: function (headerName) { return ""; },
    readyState: 0,
    responseText: "",
    responseXML: "",
    onreadystatechange: function () {},
    open: function (method, url, async, username, password) { return {}; },
    overrideMimeType: function (mimeType) { return {}; }, //Gecko
    send: function (content) {},
    setRequestHeader: function (label, value) {},
    statusText: "",
    status: 0,
};
