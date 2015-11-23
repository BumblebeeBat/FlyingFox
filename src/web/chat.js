var button = document.createElement("BUTTON");
button.id = "chat_button";
var t = document.createTextNode("load messages");
button.appendChild(t);
function chat_func() { variable_get(["msg_peers"], chat_func2); }
function chat_func2(peers) {
    console.log(peers[1]);
    variable_get(["msg_ids", peers[1]], function(x) {chat_func3(x, peers[1])} );
}
start = 0;
temp_messages = [];
messages = [];
function chat_func3(ids, partner) {
    console.log(ids);
    temp_messages = [];
    chat_func4(ids, partner, 1, ids.length);
}
function chat_func4(ids, partner, N, M) {
    if (N == M) {
	console.log("done");
	console.log(temp_messages);
    } else if (ids[N] < start) {
	chat_func4(ids, partner, N + 1, M);
    } else {
	var x = ids[N];
	if (x > start) { start = x + 1;	}
	variable_get(["read_msg", partner, x], function(msg) {
	    temp_messages = temp_messages.concat([atob(msg)]);
	    msgs = document.getElementById("messages");
	    xTimes(1, function() {
		var li = document.createElement("li");
		li.innerHTML = atob(msg);
		msgs.appendChild(li);
	    });
	    chat_func4(ids, partner, N + 1, M);
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

button.onclick = chat_func;
document.body.appendChild(button);
var scrollBox = document.createElement("div");
//scrollBox.style = "height:120px;width:120px;border:1px solid #ccc;font:16px/26px Georgia, Garamond, Serif;overflow:auto;";
scrollBox.style = "height:400px;width:500px;border:1px solid #ccc;font:14px/14px Georgia, Garamond, Serif;overflow:auto;";
var ul = document.createElement("ul");
ul.id = "messages";
scrollBox.appendChild(ul);
document.body.appendChild(scrollBox);
