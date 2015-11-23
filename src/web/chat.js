/*
<div style="height:120px;width:120px;border:1px solid #ccc;font:16px/26px Georgia, Garamond, Serif;overflow:auto;">
As you can see, once there's enough text in this box, the box will grow scroll bars... that's why we call it a scroll box! You could also place an image into the scroll box.
</div>
*/

var button = document.createElement("BUTTON");
button.id = "chat_button";
var t = document.createTextNode("load messages");
button.appendChild(t);
function doit() { variable_get(["msg_peers"], doit2); }
function doit2(peers) {
    console.log(peers[1]);
    variable_get(["msg_ids", peers[1]], function(x) {doit3(x, peers[1])} );
}
start = 0;
temp_messages = [];
messages = [];
function doit3(ids, partner) {
    console.log(ids);
    temp_messages = [];
    doit4(ids, partner, 1, ids.length);
}
function doit4(ids, partner, N, M) {
    if (N == M) {
	console.log("done");
	console.log(temp_messages);
    } else {
	var x = ids[N];
	variable_get(["read_msg", partner, x], function(msg) {
	    temp_messages = temp_messages.concat([atob(msg)]);
	    msgs = document.getElementById("messages");
	    xTimes(1, function() {
		var li = document.createElement("li");
		li.innerHTML = atob(msg);
		msgs.appendChild(li);
	    });
	    doit4(ids, partner, N + 1, M);
	}
		    );
    }
}
function xTimes(N, f) {
    if (N > 0) {
	f();
	xTimes(N-1, f);
    }
}

button.onclick = doit;
document.body.appendChild(button);
var scrollBox = document.createElement("div");
//scrollBox.style = "height:120px;width:120px;border:1px solid #ccc;font:16px/26px Georgia, Garamond, Serif;overflow:auto;";
scrollBox.style = "height:400px;width:500px;border:1px solid #ccc;font:14px/14px Georgia, Garamond, Serif;overflow:auto;";
var ul = document.createElement("ul");
ul.id = "messages";
scrollBox.appendChild(ul);
document.body.appendChild(scrollBox);
