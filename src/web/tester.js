function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

local_get(["sync", [127,0,0,1], 3020]);
local_get(["new_pubkey", btoa("abc")]);
var x = local_get(["pubkey"]);
refresh_helper(x, function(){ 
    p = JSON.parse(xml_out(x)); 
    pubkey = p[1];
    console.log("create account ".concat(pubkey));
    local_get2(["create_account", pubkey, 1000000, 50]);
    wait_for_id();
});

function wait_for_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for id");
    var x = local_get(["id"]);
    refresh_helper(x, function(){
	i = JSON.parse(xml_out(x));
	id = i[1];
	if (id == -1) {wait_for_id();}
	else {new_channel(id);}
    });
}

function new_channel(id) {
    console.log("new channel creator");
    //the channel needs to be signed by both participants before it can be published.
    local_get2(["create_channel", id, 102030, 0, btoa("delegated_1"), 50]);
    wait_for_channel_id();
}

function wait_for_channel_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for channel id");
    var x = local_get(["id"]);
    refresh_helper(x, function(){
	i = JSON.parse(xml_out(x));
	chid = i[1];
	console.log(i);
	if (chid == []) {wait_for_id();}
	else {chid[0];}
    });
    
}
