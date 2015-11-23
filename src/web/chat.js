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
	    doit4(ids, partner, N + 1, M);
	}
		    );
    }
}

button.onclick = doit;
document.body.appendChild(button);
