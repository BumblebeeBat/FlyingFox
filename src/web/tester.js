function get2(t, callback) {
    u = url(PORT + 10, "localhost");
    return getter(t, u, callback);
}
function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

function variable_get(cmd, callback) {
    var x = local_get(cmd);
    var_get(x, callback);
}
function variable_get2(cmd, callback) {
    var x = local_get2(cmd);
    var_get(x, callback);
}
function var_get(x, callback) {
    refresh_helper(x, function(){
	p = JSON.parse(xml_out(x));
	console.log(p);
	out = p[1];
	console.log(out);
	callback(out);
    });
}

local_get(["new_pubkey", btoa("abc")]);

variable_get(["pubkey"], function(pubkey) {
    local_get2(["create_account", pubkey, 1000000, 50]);
    wait_for_id();
})
function wait_for_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for id");
    variable_get(["id"], new_channel);
}
function new_channel(id) {
    if (id == -1) {variable_get(["id"], new_channel);}
    else {
	console.log("new channel creator");
	console.log("new id ".concat(id));
	//the channel needs to be signed by both participants before it can be published.
	variable_get2(["create_channel", id, 112030, 0, btoa("delegated_1"), 50], function(ch) {
	    console.log("talking to 2");
	    console.log(ch);
	    variable_get(["sign", ch], function(ch2) {
		console.log(ch2);
		variable_get2(["sign", ch2], function(ch3) {
		    console.log(ch3);
		    get(["tx_absorb", ch3]);
		    get2(["tx_absorb", ch3]);
		    wait_for_channel_id();
		});
	    });
	});
    }
}

function wait_for_channel_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for channel id");
    variable_get(["channel_id", 0], function(chid) {
	console.log("channel id ".concat(chid));
	if (chid == [-6]) {wait_for_channel_id();}
	if (chid == -6) {
	    console.log("bad");
	    wait_for_channel_id();}
	//else if (typeof chid === 'undefined') {wait_for_channel_id();}
	else {channel_spend(chid);}
    });
}

function channel_spend(chid) {
    console.log("channel spend id ".concat(chid));
    variable_get(["channel_spend", chid, -10000], function(ch) {
	console.log("channel spend ".concat(ch));
    });
}
