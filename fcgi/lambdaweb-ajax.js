function createHttpRequest() {
  if (window.XMLHttpRequest) {
    return new XMLHttpRequest();
  } else if (window.ActiveXObject) {
    return new ActiveXObject("Microsoft.XMLHTTP");
  }
}

function moveLast(child) {
	var parent = child.parentNode;
	if (parent.lastChild != child) {
		parent.removeChild(child);
		parent.appendChild(child);
	}
}

function addInput(cmd) {
  var dl = document.getElementById("output").firstChild;
  var dt = document.createElement("dt");
  dt.appendChild(document.createTextNode(cmd));
  dl.appendChild(dt);
  return dt;
}

function addOutput(resp) {
  var dl = document.getElementById("output").firstChild;

  var dd = document.createElement("dd");
  var ul = document.createElement("ul");
  dd.appendChild(ul);
  dl.appendChild(dd);

  var ls = resp.split("\n");
  for (var i = 0; i < ls.length; i++) {
    if (!ls[i].match("^\\s*$")) {
      var li = document.createElement("li");
      li.appendChild(document.createTextNode(ls[i]));
      ul.appendChild(li);
    }
  }
}

function processReqChange(req, inputElem) {
  return function () {
    if (req.readyState == 4) {
        if (req.status == 200) {
	    moveLast(inputElem);
            addOutput(req.responseText);
        } else {
            addOutput("Error: " + req.status + " " + req.statusText);
        }
    }
  }
}

function urlencode(s) {
	return escape(s).replace(/\+/g,"%2b").replace(/\%20/g,"+");
}

function send() {
  var cmdInput = document.getElementById("cmd");
  var cmd = cmdInput.value;
  cmdInput.value = "";

  var reqUrl = document.getElementById("inputForm").action + "?cmd=" + urlencode(cmd);
  var req = createHttpRequest();
  if (req) {
    var inputElem = addInput(cmd);
    req.onreadystatechange = processReqChange(req, inputElem);
    req.open("GET", reqUrl, true);
    req.send(null);
    return false;
  }

  return true;
}

function init() {
  document.getElementById("output").appendChild(document.createElement("dl"));
  document.getElementById("inputForm").onsubmit = send;
  document.getElementById("cmd").focus();
}

window.onload = init;
