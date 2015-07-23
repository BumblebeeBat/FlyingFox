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
my_port = 7780;
function getter(t, u, callback){
    t = JSON.stringify(t);
    u = u.concat(btoa(t));
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("GET",u,true);
    xmlhttp.send(null);
    return xmlhttp
}
function get(t, callback) {
    u = url(my_port, "localhost");
    return getter(t, u, callback);
}
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
function xml_check(x) { return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, callback) {
    if (xml_check(x)) {callback();}
    else {setTimeout(function() {refresh_helper(x, callback);}, 1000);}
};

my_status = {};
function refresh_status() {
    var x = get("status");
    refresh_helper(x, function(){ my_status = JSON.parse(xml_out(x)); });
};
refresh_status();
window.setInterval(refresh_status, 10000);

server_status = {};
function refresh_server_status() {
    var x = server_get("status");
    refresh_helper(x, function(){ server_status = JSON.parse(xml_out(x));});
};
refresh_server_status();
window.setInterval(refresh_server_status, 10000);

inbox_peers = [];
function refresh_inbox_peers() {
    var x = local_get("inbox_peers");
    refresh_helper(x, function(){ inbox_peers = JSON.parse(xml_out(x));});
};
refresh_inbox_peers();
window.setInterval(refresh_inbox_peers, 20000);

network_peers = [];
function refresh_network_peers() { //unused at this time.
    var x = get("all_peers");
    refresh_helper(x, function(){ network_peers = JSON.parse(xml_out(x));});
};

inbox_size = -1;
function refresh_inbox_size() {
    pub = document.getElementById("other").value;
    var x = local_get("inbox_size&".concat(pub));
    refresh_helper(x, function(){ inbox_size = xml_out(x);});
};
refresh_inbox_size();
window.setInterval(refresh_inbox_size, 2000);

channel_peers = [];
function refresh_channel_peers() {
    var x = local_get("channel_peers");
    refresh_helper(x, function(){ channel_peers = JSON.parse(xml_out(x));});
};
refresh_channel_peers();
window.setInterval(refresh_channel_peers, 5000);

channel_balance = -1;
function refresh_channel_balance() {
    var x = local_get("channel_balance&".concat(server_status.pubkey));
    refresh_helper(x, function(){ channel_balance = xml_out(x);});
    
};
refresh_channel_balance();
window.setInterval(refresh_channel_balance, 5000);

my_balance = -1;
function refresh_my_balance() {
    var x = get("kv&".concat(my_status.pubkey));
    refresh_helper(x, function(){ my_balance = JSON.parse(xml_out(x)).amount;});
};
setTimeout(refresh_my_balance, 2000);//after a short delay, that way we are sure my_status is ready
window.setInterval(refresh_my_balance, 5000);

channel = "nil";
function refresh_channel() {
    var x = local_get("channel_get&".concat(server_status.pubkey));
    refresh_helper(x, function(){ channel = JSON.parse(JSON.parse(xml_out(x)));});
};
setTimeout(refresh_channel, 3000);//after a short delay, that way we are sure server_status is ready
window.setInterval(refresh_channel, 5000);

mail_nodes = [];
function refresh_mail_nodes() {
    var x = get("mail_nodes")
    refresh_helper(x, function(){ mail_nodes = JSON.parse(JSON.parse(xml_out(x)));});
};
refresh_mail_nodes();
window.setInterval(refresh_mail_nodes, 10000);

txs = [];
function refresh_txs() {
    var x = get("txs");
    refresh_helper(x, function(){ txs = JSON.parse(xml_out(x)); });
};
refresh_txs();
window.setInterval(refresh_txs, 2000);

messages = [];
function message(index, pub) {
    x = local_get("read_message&".concat(index).concat("&").concat(pub));
    refresh_helper(x, function(){ return "success"; });
};
function refresh_messages() {
    // this could be so much more efficient.
    // we don't need to refresh the whole page every time, we can just append the new message.
    // if "other" changes then we should refresh the page.
    pub = document.getElementById("other").value;
    temp = [];
    for (i = 0; i < inbox_size; i++) { messages.push(message(i, pub)); };
    empty_messages();
    messages.map(function(m) {
	if (m === undefined)  { false; }
	else if (m.to == pub) { add_message(msg.msg); }
	else                  { recieve_message(msg.msg); }
    });
};
window.setInterval(refresh_messages, 5000);

function to_channel(pub, amount) {
    x = local_get("to_channel&".concat(pub).concat("&").concat(amount));
    refresh_helper(x, function(){ return "success";});
};
function register() {
    x = local_get("register&".concat(JSON.stringify(server())))
    refresh_helper(x, function(){return xml_out(x) ;} );
};
document.getElementById("send_button").onclick=function() {
    var msg = document.getElementById("msg").value;
    node = server();
    pub = document.getElementById("other").value;
    URL = "send_message&".concat(JSON.stringify(node)).concat("&").concat(pub).concat("&").concat(JSON.stringify(msg));
    //console.log("url ".concat(URL));
    local_get(URL);
    refresh();
};
peer_id = 0;
document.getElementById("switch_partner").onclick=function() {
    //ps = JSON.parse(JSON.parse(local_get("inbox_peers")));
    ps = JSON.parse(inbox_peers);
    //console.log("switch partner ps ".concat(ps));
    peer_id = peer_id + 1;
    if (ps.length > 0) {
	document.getElementById("other").value = ps[peer_id % peers.length];
    };
};

function refresh_balance() {
    pub = my_status.pubkey;
    document.getElementById("pub").innerHTML = "pubkey: ".concat(pub);
    bal2 = channel_balance;
    //console.log("channel balance: ".concat(bal2));
    amount = my_balance;
    // document.getElementById("bal").innerHTML = "balance: ".concat(amount);
    document.getElementById("bal").innerHTML = "balance: ".concat(bal2);
}
function refresh() {
    v = document.getElementById("other").value;
    //console.log("partner ".concat(v));
    refresh_balance();
    refresh_messages();
}

// we need a function for loading new pub/priv pairs into the node.

function server() {return {"ip":"45.55.5.85","port":PORT,"__struct__":"Elixir.Peer"}};

function register_1() {
    //did we make the channel yet? if not, make it, and update registeration status. recurse.
    s = server_status;
    pub = s.pubkey;
    s = my_status;
    my_pub = s.pubkey;
    if (channel == "nil") {
	console.log("my balance ".concat(my_balance));
	amount = Math.min(3000000, parseInt(my_balance));
	console.log("amount: ".concat(amount));
	txss = txs.filter(function (tx) {
	    return tx.data.__struct__ == ":Elixir.ToChannel";
	});
	txss = txss.filter(function (tx) {
	    console.log("error error");
	    return tx.data.pub == my_pub;;
	});
	console.log("txs ".concat(JSON.stringify(txss.length)));
	console.log("txs ".concat(JSON.stringify([].length)));
	console.log("txs ".concat([].length));
	if (txss.length == 0) { // tx isn't in txs
	    console.log("create channel");
	    console.log("amount ".concat(amount));
	    to_channel(pub, amount);
	}
    }
    document.getElementById("reg").innerHTML = "wait for next block";
    return register_2(pub);
}

function register_2(pub) {
    // is the channel ready to use yet? if not, wait a while, then recurse.
    // did we register the mailnode yet? if not, do it, and update registration status and exit.
    m = mail_nodes.length == 0;
    //console.log("mail_nodes ".concat(JSON.stringify(mail_nodes.length)));
    //console.log("m ".concat(m));
    //console.log("[] ".concat(JSON.stringify([].length)));
    if (channel == "nil") {
	console.log("no channel");
	setTimeout(function() {register_2(pub);}, 3000);
    } else if (m) {
	console.log("mail node needs to be registered");
	register();
	document.getElementById("reg").innerHTML = "registered";
    } else {
	console.log("mail node works");
	document.getElementById("reg").innerHTML = "registered";
    }
}

setTimeout(register_1, 4000);
window.setInterval(refresh, 5000);
</script>
    </html>
