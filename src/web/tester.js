function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

local_get([-6, "sync", [-6,127,0,0,1], 3020]);
local_get([-6, "new_pubkey", btoa("abc")]);
var x = local_get([-6, "pubkey"]);
refresh_helper(x, function(){ 
    p = JSON.parse(xml_out(x)); 
    pubkey = p[2];
    console.log("create account ".concat(pubkey));
    local_get2([-6, "create_account", pubkey, 1000000, 50]);
    wait_for_id();
});

function wait_for_id() {
    local_get([-6, "sync", [-6,127,0,0,1], 3020]);
    console.log("wait for id");
    var x = local_get([-6, "id"]);
    refresh_helper(x, function(){
	i = JSON.parse(xml_out(x));
	id = i[2];
	if (id == -1) {wait_for_id();}
	else {new_channel(id);}
    });
}

function new_channel(id) {
    console.log("new channel creator");
    //the channel needs to be signed by both participants before it can be published.
    local_get2([-6, "create_channel", id, 102030, 0, btoa("delegated_1"), 50]);
}
