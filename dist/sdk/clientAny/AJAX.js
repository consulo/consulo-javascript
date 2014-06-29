window.ActiveXObject = function() {};//XMLHttpRequest
window.XmlHttpRequest = function() {};//XMLHttpRequest
window.prototype = new Object();

var XMLHttpRequest = {
	onreadystatechange: function () {}
};
XMLHttpRequest.constructor = 0;//Object
XMLHttpRequest.readyState = 0;//Number
XMLHttpRequest.responseText = 0;//String
XMLHttpRequest.statusText = 0;//String
XMLHttpRequest.status = 0;//Number
XMLHttpRequest.responseXML = 0;//String
XMLHttpRequest.abort = function() {};//Object
XMLHttpRequest.getAllResponseHeaders = function() {};//String
XMLHttpRequest.getResponseHeader = function(headerName) {};//String
XMLHttpRequest.overrideMimeType = function(mimeType) {};//Gecko,Object
XMLHttpRequest.open = function(method,url,async,username,password) {};//Object
XMLHttpRequest.send = function(content) {};//void
XMLHttpRequest.setRequestHeader = function(label,value) {};//void

XMLHttpRequest.prototype = new Object();
