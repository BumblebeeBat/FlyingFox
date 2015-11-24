var chat_button = document.createElement("BUTTON");
chat_button.id = "chat_button";
var t = document.createTextNode("load messages");
chat_button.appendChild(t);
function chat_func() { variable_get(["msg_peers"], chat_func2); }
function chat_func2(peers) {
    console.log(peers[1]);
    variable_get(["msg_ids", peers[1]], function(x) {chat_func3(x, peers[1])} );
}
start = 0;
function chat_func3(ids, partner) {
    console.log(ids);
    chat_func4(ids, partner, 1, ids.length);
}
function chat_func4(ids, partner, N, M) {
    if (N == M) {
	console.log("done");
    } else if (ids[N] < start) {
	chat_func4(ids, partner, N + 1, M);
    } else {
	var x = ids[N];
	if (x > start) { start = x + 1;	}
	variable_get(["read_msg", partner, x], function(msg) {
	    msgs = document.getElementById("messages");
	    var li = document.createElement("li");
	    li.innerHTML = atob(msg);
	    msgs.appendChild(li);
	    chat_func4(ids, partner, N + 1, M);
	}
		    );
    }
}

chat_button.onclick = chat_func;
document.body.appendChild(chat_button);
var scrollBox = document.createElement("div");
//scrollBox.style = "height:120px;width:120px;border:1px solid #ccc;font:16px/26px Georgia, Garamond, Serif;overflow:auto;";
scrollBox.style = "height:400px;width:500px;border:1px solid #ccc;font:14px/14px Georgia, Garamond, Serif;overflow:auto;";
var ul = document.createElement("ul");
ul.id = "messages";
scrollBox.appendChild(ul);
document.body.appendChild(scrollBox);
