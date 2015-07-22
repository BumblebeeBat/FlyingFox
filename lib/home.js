    <html>
    <font id="pub"></font> <br>
    <font id="bal"></font> <br>
    <font>registration status: </font><font id="reg">"not registered"</font> <br>
    <font>partner pubkey: </font><textarea id="other" rows="1" cols="80">a</textarea>
    <button type="button" id="switch_partner">Switch Partner</button> <br>
    <textarea id="msg" rows="6" cols="80"></textarea>
    <button type="button" id="send_button">Send Message</button>
    <ul id="messages" style="list-style: none; padding: 0; margin: 0;"></ul>
    <head>
    </head>
    <script>
    // we neeed to add a button near the partner pubkey. it should grab Cli.inbox_peers, and rotate through that.

function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
PORT = 46666;

// my_port = 7668;
my_port = 7778;
function getter(u){
    xmlhttp=new XMLHttpRequest();
    xmlhttp.open("GET",u,false);
    xmlhttp.send();
    out = xmlhttp.responseText;
    return out;
}
function get(s) {
    u = url(my_port, "localhost");
    s = JSON.stringify(s);
    // console.log("get ".concat(s));
    u = u.concat(btoa(s));
    return getter(u);
}
function server_get(t) {
    s = server();
    u = url(s.port, s.ip);
    t = JSON.stringify(t);
    u = u.concat(btoa(t));
    // console.log("server_get ".concat(u));
    return getter(u);
}
function local_get(s) {
    u = url(my_port + 1000, "localhost");
    u = u.concat("priv/");
    s = JSON.stringify(s);
    u = u.concat(btoa(s));
    // console.log("local get ".concat(u));
    return getter(u);
}
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
function empty_messages() {
    var ul = document.getElementById("messages");
    ul.innerHTML = '';
}
function peers() { return JSON.parse(local_get("inbox_peers"));

		 } 
function network_peers() { return JSON.parse(get("all_peers")); } 
function message(index, pub) {return JSON.parse(local_get("read_message&".concat(index).concat("&").concat(pub))); }
function refresh_messages() {
    empty_messages();
    pub = peers()[0];
    // console.log(pub.concat(" our peer"));
    size = local_get("inbox_size&".concat(pub));
    //console.log("size ".concat(size));
    for (i = 0; i < size; i++) {
	msg = message(i, pub);
	if (msg.to == pub) {
	    add_message(msg.msg);
	} else {
	    recieve_message(msg.msg);
	}
    }
}
document.getElementById("send_button").onclick=function() {
    var msg = document.getElementById("msg").value;
    node = server();
    pub = document.getElementById("other").value;
    URL = "send_message&".concat(JSON.stringify(node)).concat("&").concat(pub).concat("&").concat(JSON.stringify(msg));
    console.log("url ".concat(URL));
    b = local_get(URL);
    refresh();
};
peer_id = 0;
document.getElementById("switch_partner").onclick=function() {
    ps = JSON.parse(JSON.parse(local_get("inbox_peers")));
    peer_id = peer_id + 1;
    if (ps.length > 0) {
	document.getElementById("other").value = ps[peer_id % peers.length];
    };
};

function refresh_balance() {
    status = get("status");
    pub = JSON.parse(status).pubkey;
    document.getElementById("pub").innerHTML = "pubkey: ".concat(pub);
    acc = get("kv&".concat(pub));
    cpeers = JSON.parse(local_get("channel_peers"));
    console.log(cpeers[0]);
    
    bal2 = local_get("channel_balance&".concat(cpeers[0]));
    console.log("channel balance: ".concat(bal2));
    

    amount = JSON.parse(acc).amount;
    // document.getElementById("bal").innerHTML = "balance: ".concat(amount);
    document.getElementById("bal").innerHTML = "balance: ".concat(bal2);
}
function refresh() {
    v = document.getElementById("other").value;
    console.log("partner ".concat(v));
    refresh_balance();
    refresh_messages();
}

// we need a function for loading new pub/priv pairs into the node.

function server() {
    p = network_peers()[0];
    p.ip = "45.55.5.85";
    p.port = PORT;
    return p;
}

function register_1() {
    //did we make the channel yet? if not, make it, and update registeration status. recurse.
    s = JSON.parse(server_get("status"));
    pub = s.pubkey;
    s = JSON.parse(get("status"));
    my_pub = s.pubkey;
    console.log("pub ".concat(pub))
    channel = JSON.parse(JSON.parse(get("channel_get&".concat(pub))));
    console.log("channel ".concat(JSON.stringify(channel)));
    // console.log("channel ".concat(JSON.parse(channel)));
    if (channel == "nil") {
	balance = JSON.parse(get("kv&".concat(my_pub))).amount;
	console.log("balance: ".concat(balance));
	amount = Math.min(3000000, parseInt(balance));
	console.log("amount: ".concat(amount));
	//need to check mempool for the tx
	txs = JSON.parse(get("txs"));
	// console.log("txs ".concat(JSON.stringify(txs)));
	txs = txs.filter(function (tx) {
	    // console.log("tx ".concat(JSON.stringify(tx)));
	    // console.log("tx ".concat(tx.data.__struct__));
	    return tx.data.__struct__ == ":Elixir.ToChannel";
	})
	txs = txs.filter(function (tx) {
	    return tx.data.pub == my_pub;;
	});
	console.log("txs ".concat(JSON.stringify(txs.length)));
	console.log("txs ".concat(JSON.stringify([].length)));
	console.log("txs ".concat([].length));
	if (txs.length == 0) { // tx isn't in txs
	    console.log("create channel");
	    console.log("amount #{inspect amount}")
	    local_get("to_channel&".concat(pub).concat("&").concat(amount));
	}
    }
    document.getElementById("reg").innerHTML = "wait for next block";
    return register_2(pub);
}

function register_2(pub) {
    // is the channel ready to use yet? if not, wait a while, then recurse.
    // did we register the mailnode yet? if not, do it, and update registration status and exit.
    channel = JSON.parse(JSON.parse(get("channel_get&".concat(pub))));
    mail_nodes = JSON.parse(JSON.parse(get("mail_nodes")));
    m = mail_nodes.length == 0;
    console.log("mail_nodes ".concat(JSON.stringify(mail_nodes.length)));
    console.log("m ".concat(m));
    console.log("[] ".concat(JSON.stringify([].length)));
    if (channel == "nil") {
	console.log("no channel");
	setTimeout(function() {register_2(pub);}, 3000);
    } else if (m) {
	console.log("mail node needs to be registered");
	s = server();
	local_get("register&".concat(JSON.stringify(s)));
	document.getElementById("reg").innerHTML = "registered";
    } else {
	console.log("mail node works");
	document.getElementById("reg").innerHTML = "registered";
    }
}

s = get("status");
console.log("local status ".concat(s));
register_1();
window.setInterval(refresh, 5000);
</script>
    </html>
