    <html>
    <head>
    </head>
    <body>
    <div id="main"></div>
    </body>
    <script>

function set_interval(l2, f, t) {
    if (lock_status == l2){
	f();
	setTimeout(function(){set_interval(l2, f, t)}, t);
    }
};
function interval_unlocked(f, t) { set_interval("unlocked", f, t); };//this function is so that when we change back to locked mode, all the threads that we don't need turn off.
state = "none";
function unlocked() {
    if (state !== "unlocked") { unlocked_2(); };
}
function unlocked_2() {
    state = "unlocked";
    var div = document.getElementById("main");
    div.innerHTML = "";
    
    refresh_server_status();
    interval_unlocked(refresh_server_status, 2000);
    refresh_inbox_peers();
    interval_unlocked(refresh_inbox_peers, 2000);
    refresh_channel_peers();
    interval_unlocked(refresh_channel_peers, 2000);
    //setTimeout(refresh_channel, 3000);//after a short delay, that way we are sure server_status is ready
    interval_unlocked(refresh_channel, 2000);
    refresh_mail_nodes();
    interval_unlocked(refresh_mail_nodes, 2000);
    refresh_txs();
    interval_unlocked(refresh_txs, 2000);
    register_1();

    var x = document.createElement("font");
    x.id = "height";
    div.appendChild(x);
    refresh_height();
    interval_unlocked(refresh_height, 2000);

    var x = document.createElement("font");
    x.id = "pub";
    div.appendChild(x);
    refresh_status();
    interval_unlocked(refresh_status, 2000);

    var x = document.createElement("font");
    x.id = "bal";
    div.appendChild(x);
    setTimeout(refresh_my_balance, 1000);
    interval_unlocked(refresh_my_balance, 5000);
    
    var x = document.createElement("font");
    x.id = "cbal";
    div.appendChild(x);
    refresh_channel_balance();
    interval_unlocked(refresh_channel_balance, 5000);
    
    var x = document.createElement("font");
    x.id = "reg";
    x.innerHTML = "registration status: not registered<br>";
    div.appendChild(x);
    
    var x = document.createElement("font");
    var t = document.createTextNode("partner's pubkey: ");
    x.appendChild(t);
    div.appendChild(x);
    
    var x = document.createElement("textarea");
    var t = document.createTextNode("BOyZPCA8xuXNaxmqSZaTHPumJiOY8TWt23LjTWLGk1PGY178HeCqY82qY4YAX01zcLrFREdiDv33TlL8VxG9ws0");
    x.id = "other";
    x.rows = "1";
    x.cols = "80";
    x.appendChild(t);
    div.appendChild(x);
    refresh_inbox_size();
    other = document.getElementById("other").innerHTML;
    console.log("other ".concat(other));
    other = document.getElementById("other").value;
    console.log("other ".concat(other));
    interval_unlocked(refresh_inbox_size, 1000);

    var x = document.createElement("button");
    x.type = "button";
    x.id = "switch_partner";
    var t = document.createTextNode("Switch Partner");
    x.appendChild(t);
    div.appendChild(x);
    peer_id = 0;
    x.onclick=function() {
	peer_id = (peer_id + 1) % inbox_peers.length;
	if (inbox_peers.length > 0){document.getElementById("other").value = inbox_peers[peer_id];};
    };

   
    var x = document.createElement("br");
    div.appendChild(x);
    
    var x = document.createElement("textarea");
    x.id = "msg";
    x.rows = 6;
    x.cols = 80;
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    x.id = "send_button";
    var t = document.createTextNode("Send Message");
    x.appendChild(t);
    div.appendChild(x);
    document.getElementById("send_button").onclick=function() {
	var msg = document.getElementById("msg").value;
	pub = document.getElementById("other").value;
	var node = JSON.stringify(server());
	msg = JSON.stringify(msg);
	URL = ["send_message", node, pub, msg];
	console.log("url ".concat(URL));
	local_get(URL);
	document.getElementById("msg").value = "";
    };

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("ul");
    x.id = "messages";
    x.style = "list-style: none; padding: 0; margin: 0;";
    div.appendChild(x);

    interval_unlocked(refresh_messages_3, 1000);
    // window.setInterval(refresh_messages_3, 1000);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    x.id = "delete";
    var t = document.createTextNode("Delete Messages");
    x.appendChild(t);
    div.appendChild(x);
    document.getElementById("delete").onclick=function() {
	pub = document.getElementById("other").value;
	URL = ["delete_all_messages", pub];
	local_get(URL);
	refresh_messages();
    };

    var x = document.createElement("button");
    x.type = "button";
    var t = document.createTextNode("Lock");
    x.appendChild(t);
    div.appendChild(x);
    x.onclick=function() {
	refresh_page();
	local_get(["lock"]);
    };
};
//setTimeout(unlocked, 1000);
function locked() {
    if (state !== "locked") { locked_2();} ;
}
function locked_2() {
    state = "locked";
    console.log("locked function");
    var div = document.getElementById("main");
    div.innerHTML = "";

    var x = document.createElement("font");
    var t = document.createTextNode("passphrase to unlock: ");
    x.appendChild(t);
    div.appendChild(x);

    var x = document.createElement("textarea");
    x.id = "passphrase";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    var t = document.createTextNode("Unlock");
    x.appendChild(t);
    div.appendChild(x);
    x.onclick=function() {
	passphrase = document.getElementById("passphrase").value;
	refresh_page();
	local_get(["unlock", passphrase]);
    };

    var x = document.createElement("button");
    x.type = "change_password";
    var t = document.createTextNode("ChangePassword");
    x.appendChild(t);
    div.appendChild(x);
    x.onclick=function() {
	change_password();
    };
};
function change_password() {
    state = "change_password";
    console.log("change_password function");
    var div = document.getElementById("main");
    div.innerHTML = "";

    var x = document.createElement("font");
    var t = document.createTextNode("old passphrase: ");
    x.appendChild(t);
    div.appendChild(x);
    var x = document.createElement("textarea");
    x.id = "old";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("font");
    var t = document.createTextNode("new passphrase: ");
    x.appendChild(t);
    div.appendChild(x);
    var x = document.createElement("textarea");
    x.id = "new";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("font");
    var t = document.createTextNode("confirm new passphrase: ");
    x.appendChild(t);
    div.appendChild(x);
    var x = document.createElement("textarea");
    x.id = "confirm";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    var t = document.createTextNode("Update Passphrase");
    x.appendChild(t);
    x.onclick=function() {
	old = document.getElementById("old").value;
	new_pass = document.getElementById("new").value;
	confirm_pass = document.getElementById("confirm").value;
	if (new_pass === confirm_pass) {
	    refresh_page();
	    local_get(["change_password_key", old, new_pass]);
	    //unlocked();
	};
    };
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    var t = document.createTextNode("cancel");
    x.appendChild(t);
    x.onclick=function() {
	locked();
	//refresh_page();
    };
    div.appendChild(x);
};
function empty() {
    if (state !== "empty") { empty_2(); };
}

function empty_2(){
    state = "empty";
    var div = document.getElementById("main");
    div.innerHTML = "";

    var x = document.createElement("font");
    var t = document.createTextNode("Creating new account.");
    x.appendChild(t);
    div.appendChild(x);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("font");
    var t = document.createTextNode("choose a passphrase: ");
    x.appendChild(t);
    div.appendChild(x);

    var x = document.createElement("textarea");
    x.id = "passphrase";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("br");
    div.appendChild(x);

    var x = document.createElement("font");
    var t = document.createTextNode("confirm passphrase: ");
    x.appendChild(t);
    div.appendChild(x);

    var x = document.createElement("textarea");
    x.id = "confirm";
    x.rows = "1";
    x.cols = "80";
    div.appendChild(x);

    var x = document.createElement("button");
    x.type = "button";
    var t = document.createTextNode("Unlock");
    div.appendChild(x);
    x.onclick=function() {
	passphrase = document.getElementById("passphrase").value;
	confirm = document.getElementById("confirm").value;
	if (passphrase === confirm) {
	    refresh_page();
	    local_get(["change_password_key", "", passphrase]);
	};
    };

};


// we need a function for loading new pub/priv pairs into the node.
function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
PORT = 46666;
my_port = 46666;
function xml_check(x) { return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, callback) {
    if (xml_check(x)) {callback();}
    else {setTimeout(function() {refresh_helper(x, callback);}, 1000);}
};
function getter(t, u, callback){
    t = JSON.stringify(t);
    //console.log("getter ".concat(t));
    //u = u.concat(btoa(t)); // don't append to the url any more.
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("POST",u,true);
    xmlhttp.send(t);
    return xmlhttp
}
function get(t, callback) {
    u = url(my_port, "localhost");
    return getter(t, u, callback);
}
function server() {return {"ip":"45.55.5.85","port":PORT,"__struct__":"Elixir.Peer"}};
function server_get(t, callback) {
    s = server();
    u = url(s.port, s.ip);
    return getter(t, u, callback);
}
function local_get(t, callback) {
    u = url(my_port + 1000, "localhost");
    u = u.concat("priv/");
    return getter(t, u, callback);
}
function empty_messages() {
    var ul = document.getElementById("messages");
    ul.innerHTML = "";
}

height = -1;
function refresh_height() {
    var x = get(["kv", "height"]);
    refresh_helper(x, function(){ height = JSON.parse(xml_out(x)); });
    document.getElementById("height").innerHTML = "height: ".concat(height).concat("<br>");
    //console.log("height ".concat(height));
};

my_status = {};
function refresh_status() {
    var x = get(["status"]);
    refresh_helper(x, function(){ my_status = JSON.parse(xml_out(x)); });
    document.getElementById("pub").innerHTML = "pubkey: ".concat(my_status.pubkey).concat("<br>");
    //console.log("my status ".concat(JSON.stringify(my_status)));
};

server_status = "nil";
function refresh_server_status() {
    var x = server_get(["status"]);
    refresh_helper(x, function(){ server_status = JSON.parse(xml_out(x));});
    //console.log("server status ".concat(JSON.stringify(server_status)));
};

inbox_peers = [];
function refresh_inbox_peers() {
    var x = local_get(["inbox_peers"]);
    refresh_helper(x, function(){ inbox_peers = JSON.parse(JSON.parse(xml_out(x)));});
    // console.log("inbox_peers ".concat(JSON.stringify(inbox_peers)));
};

network_peers = [];
function refresh_network_peers() { //unused at this time.
    var x = get(["all_peers"]);
    refresh_helper(x, function(){ network_peers = JSON.parse(xml_out(x));});
    console.log("network_peers ".concat(JSON.stringify(network_peers)));
};

inbox_size = -1;
function refresh_inbox_size() {
    pub = document.getElementById("other").value;
    var x = local_get(["inbox_size", pub]);
    refresh_helper(x, function(){
	old_size = inbox_size;
	inbox_size = xml_out(x);
	if (old_size !== inbox_size) { refresh_messages(); };
    });
    //console.log("inbox_size ".concat(inbox_size));
};

channel_peers = [];
function refresh_channel_peers() {
    var x = local_get(["channel_peers"]);
    refresh_helper(x, function(){ channel_peers = JSON.parse(xml_out(x));});
    //console.log("channel peers ".concat(channel_peers));
};

channel_balance = -1;
function refresh_channel_balance() {
    if (server_status !== "nil") {
	var x = local_get(["channel_balance", server_status.pubkey]);
	refresh_helper(x, function(){ channel_balance = xml_out(x);});
	document.getElementById("cbal").innerHTML = "channel balance: ".concat(channel_balance).concat("<br>");
    };
};

my_balance = -1;
function refresh_my_balance() {
    var x = get(["kv", my_status.pubkey]);
    refresh_helper(x, function(){ my_balance = JSON.parse(xml_out(x)).amount; });
    if (my_balance > 0) {
	document.getElementById("bal").innerHTML = "balance: ".concat(my_balance).concat("<br>");
    } else {
	document.getElementById("bal").innerHTML = "";
    }
};

channel = "nil";
function refresh_channel() {
    console.log("refresh channel ");
    if (server_status !== "nil") {
	var x = local_get(["channel_get", server_status.pubkey]);
	refresh_helper(x, function(){ channel = JSON.parse(JSON.parse(xml_out(x)));});
	//console.log("channel ".concat(JSON.stringify(channel)));
    };
};

mail_nodes = [];
function refresh_mail_nodes() {
    //console.log("mail nodes 1");
    var x = get(["mail_nodes"])
    refresh_helper(x, function(){ mail_nodes = JSON.parse(JSON.parse(xml_out(x)));});
    //console.log("mail nodes ".concat(JSON.stringify(mail_nodes)));
};
//setInterval(refresh_mail_nodes, 3000);

txs = [];
function refresh_txs() {
    var x = get(["txs"]);
    refresh_helper(x, function(){ txs = JSON.parse(xml_out(x)); });
};

lock_status = "";
function refresh_lock_status() {
    var x = local_get(["key_status"]);
    refresh_helper(x, function(){ lock_status = JSON.parse(xml_out(x)); });
    //console.log("lock status ".concat(lock_status));
};
setInterval(refresh_lock_status, 500);
function refresh_page() {
    if (lock_status == "locked") { locked() };
    if (lock_status == "unlocked") { unlocked() };
    if (lock_status == "empty") { empty() };
}
setInterval(refresh_page, 500);

messages = [];
function refresh_messages() {
    messages = [];
    console.log("refresh messages");
    for (i = 0; i < inbox_size; i++) {
	refresh_messages_2(i);
    };
};
function refresh_messages_2(i) {
    pub = document.getElementById("other").value;
    var x = local_get(["read_message", i, pub]);
    refresh_helper(x, function(){
	msg = JSON.parse(JSON.parse(xml_out(x)));
	messages[inbox_size - i] = msg;
    });
};
function add_message(message) {
    var ul = document.getElementById("messages");
    var li = document.createElement("li");
    li.innerHTML = '<font color="#000000">'.concat(message).concat('</font>');
    ul.appendChild(li);
}
function recieve_message(message) {
    var ul = document.getElementById("messages");
    var li = document.createElement("li");
    li.innerHTML = '<font color="#FF0000">____'.concat(message).concat('</font>');
    ul.appendChild(li);
}
function refresh_messages_3() {
    pub = document.getElementById("other").value;
    empty_messages();
    messages.map(function(m) {
	if (m === undefined)  { false; }
	else if (m.to == pub) { add_message(m.msg); }
	else                  { recieve_message(m.msg); }
    });
};

function to_channel(pub, amount) {
    x = local_get(["to_channel", pub, amount]);
    refresh_helper(x, function(){ return "success";});
};
function register() {
    x = local_get(["register", JSON.stringify(server())]); // I think this one is broken.
    refresh_helper(x, function(){return xml_out(x) ;} );
};

function register_1() {
    //did we make the channel yet? if not, make it, and update registeration status. recurse.
    s = server_status;
    console.log("server status ".concat(JSON.stringify(s)));
    pub = s.pubkey;
    s = my_status;
    my_pub = s.pubkey;
    if (my_balance == undefined || s == "nil" || my_balance < 10000) {
	setTimeout(register_1, 2000);
    } else if (channel == "nil") {
	amount = 10000000;
	txss = txs.filter(function (tx) {
	    return tx.data.__struct__ == "Elixir.ToChannel";
	});
	txss = txss.filter(function (tx) {
	    return tx.data.pub == my_pub;
	});
	if (txss.length == 0) { // tx isn't in txs
	    console.log("my balance ".concat(my_balance));
	    console.log("amount ".concat(amount));
	    console.log("create channel");
	    a = Math.min(amount, my_balance - 10000);
	    console.log("create channel with amount ".concat(a));
	    to_channel(pub, Math.min(amount, my_balance - 10000));
	}
    }
    document.getElementById("reg").innerHTML = "registration status: wait for next block".concat("<br>");
    return register_2(pub);
}

function register_2(pub) {
    // is the channel ready to use yet? if not, wait a while, then recurse.
    // did we register the mailnode yet? if not, do it, and update registration status and exit.
    if (channel == "nil") {
	setTimeout(function() {register_2(pub);}, 3000);
    } else if (mail_nodes.length == 0) {
	console.log("mail node needs to be registered");
	setTimeout(function(){ //if they don't have the block yet, then your registration channel-tx will look invalid.
	    register();
	    document.getElementById("reg").innerHTML = "registration status: registered".concat("<br>"); },
		   5000);
    } else {
	console.log("mail node works");
	document.getElementById("reg").innerHTML = "registration status: registered".concat("<br>");
    }
}

</script>
    </html>
