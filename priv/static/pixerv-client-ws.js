var VERSION = "0.1.3 alpha";

var hexDigits = new Array
	("0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"); 

//Function to convert hex format to a rgb color
function rgb2hex(rgb) {
 rgb = rgb.match(/^rgb\((\d+),\s*(\d+),\s*(\d+)\)$/);
 return hex(rgb[1]) + hex(rgb[2]) + hex(rgb[3]);
}

function hex(x) {
	return isNaN(x) ? "00" : hexDigits[(x - x % 16) / 16] + hexDigits[x % 16];
}

var version = 0;
var preferUniversalUpdates = false;
var autoUpdate = true;
var autoUpdateDuration = 1500;
var rebootLocation = "/";

function reboot() {
	window.location = rebootLocation;
}

function onClickFancyColorButton(button) {
  $(".color")[0].color.fromString($(button).text());
}

function updateFancyColors() {
  var  colors = ["354F78","6C9FF9","ECE644","C87526"];
   var h = "";
    for (var c in colors) {
      if (!colors[c]) {
        continue;
      }
      h += "<button onclick=\"onClickFancyColorButton(this);\" style=\"background:#"+colors[c]+"\" >" + colors[c] + "</button>";
    }
    $("#fancyColors").html(h);
}

function checkForControlData(packet) {
	if (packet.control.autoUpdate) {
		autoUpdate = packet.control.autoUpdate;
	}
	if (packet.control.autoUpdateDuration) {
		autoUpdateDuration = parseInt(packet.control.autoUpdateDuration);
	}
	if (packet.control.rebootLocation) {
		rebootLocation = packet.control.rebootLocation;
	}
	if (packet.control.reboot) {
		reboot();
	}
	serviceMessage(packet.control.serviceMessage);
}

function serviceMessage(message) {
	if ($("#serviceMessage").length == 0) {
		// ensure #serviceMessage exists
		$("#pixels").before("<div id=\"serviceMessage\" style=\"font-size:20px;" +
                        "font-family:sans-serif;padding:20px;background:magenta;\"></div>");
		$("#serviceMessage").hide();
	}
	if (message) {
		$("#serviceMessage").html(message);
		$("#serviceMessage").show(200);
	} else {
		$("#serviceMessage").hide(200);
	}
}

var mouseIsDown = false;
var eyeDropper = false;
var outgoingChanges = [];
var postChangesTimer;
var tellGoogleAboutDrawing = false;
var tellGoogleAboutDrawingTimer;

function colourTheCell() {
	var cellNameParts = this.id.split("-");
	var x = cellNameParts[1];
	var y = cellNameParts[2];
	var cell = $("#" + this.id);
	
	if (eyeDropper) {
		$("input")[0].color.fromString(rgb2hex(cell.css("background-color")));
	} else {
		var c = $("input")[0].value;
 		cell.css("background", "#" + c);
		sendTxt([{x:Number(x), y:Number(y), c:c}]);
	}
};

function setup() {
	  $("head").append(
        "<link href='http://fonts.googleapis.com/css?family=Press+Start+2P' rel='stylesheet' type='text/css' />" +
            "<link href='/static/pixelparty.css' rel='stylesheet' type='text/css' />");
	  
	  var html = "";
		html += "<script type=\"text/javascript\" src=\"/static/jscolor/jscolor.js\"></script><div id=\"main\">"+
		"<div class=\"stats\"><a target=\"pixelparty.me.history\" href=\"http://goo.gl/OnRGQ\">History</a></div>" +
		"<div class=\"stats\"><a target=\"pixelparty.me.stats\" href=\"http://goo.gl/TbLVN\">Live Stats</a></div>" +
		"<div>Welcome to <b><span style=\"color:red;\">Pixel</span><span style=\"color:orange;\">Party</span></b>!</div>" +
		"<div style=\"margin-bottom:3px;\"><input class=\"color\" value=\"666\" /> &lArr; <div id=\"fancyColors\"></div></div>" +
		"<table id=\"pixels\">";
		for (var y=0; y<48; y++) {
			html += "<tr>";
			for (var x=0; x<64; x++) {
				var cellName = "cell-" + x + "-" + y;
				html += "<td class=\"cell\" id=\""+cellName+"\">" + "</td>";
			}
			html += "</tr>";
		}
		html += "</table>" + 
			"<div id=\"status\"></div>" +
			"<div id=\"changes\" style=\"opacity:0.3;\"></div>" + 
			"<div id=\"delta\" style=\"opacity:0.3;\"></div>" +
			"</div>"; // #main
    $("body").append(html);
}

function drawAllCells(packet){
		checkForControlData(packet);
		var data = packet.pixels;
		version = packet.version;
		for (var y=0; y<data.length; y++) {
				for (var x=0; x<data[y].length; x++) {
					  var cell = $("#cell-" + x + "-" + y);
					  cell.css("background", "#" + data[y][x]);
				}
		}
		
    $("#loading").hide();
	  updateFancyColors();

		setTimeout(
        function() {
			      $("#status").text("Version " + VERSION + " (OK)");
			      jscolor.dir = "http://jscolor.com/jscolor/";
			      jscolor.bind("color");
			      $(".cell").mousedown(function onCellClick(ev) {
				            mouseIsDown = true;
				            eyeDropper = ev.shiftKey;
				            colourTheCell.call(this);
			          })
			          .mouseup(function oncellrelease(ev) { mouseIsDown = false; })
			          .hover(function onhover(ev) {
				            if (mouseIsDown) {
					              colourTheCell.call(this);
				            }
			          });

			      $("body").mouseup(function bodyMouseup() { mouseIsDown = false; });
		    }, 1000);
}

function updateCells(packet){
    checkForControlData(packet);
			
		var changes = packet.changes; // {"control":{},"changes":[]}
		for (var i=0; i < changes.length; i++) {
				var ch = changes[i]; // ch is the incoming change
				var cell = $("#cell-" + ch.x + "-" + ch.y); 
				version = ch.v; // update our version, since we have looked at this change
				
				cell.css("background", "#" + ch.c);
		};
		$("#status").text("Version " + VERSION + " (OK)");
		$("#delta").text(version + " delta");
}

/***  Websocket ***/

var websocket;

function init() {
    if(!("WebSocket" in window)){
        // here falls back to AJAX stuff
        alert("websockets are not supported");
    } else {
        connect();
    };
};

function connect()
{
    var wsHost = "ws://localhost:8080/websocket";
    websocket = new WebSocket(wsHost);
    websocket.onopen = function(evt) { onOpen(evt) }; 
    websocket.onclose = function(evt) { onClose(evt) }; 
    websocket.onmessage = function(evt) { onMessage(evt) }; 
    websocket.onerror = function(evt) { onError(evt) }; 
};  

function disconnect() {
    websocket.close();
}; 


function sendTxt(obj) {
    var txt = JSON.stringify(obj);
    if(websocket.readyState == websocket.OPEN){
        websocket.send(txt);
        // console.log('sending: ' + txt); 
    } else {
        console.log('websocket is not connected'); 
    };
};

function onOpen(evt) { 
    console.log('CONNECTED')
};  

function onClose(evt) {
    console.log('DISCONNECTED')
};  

function onMessage(evt) {
    // whole canvas
    var data = JSON.parse(evt.data);
    if(data.pixels){
        drawAllCells(data);
    }
    if(data.changes){
        updateCells(data);
    }
};  

function onError(evt) {
    console.log('ERROR', evt);
}


/***  On document ready ***/
$(document).ready(setup);
$(document).ready(init);
