function local_get2(t, callback) {
    u = url(3021, "localhost");
    return getter(t, u, callback);
}
//local_get(["new_pubkey", btoa("abc")]);
function register_doit() {
    variable_get(["pubkey"], function(pubkey) {
	local_get2(["create_account", pubkey, 20000000, 50]);
	setTimeout(function() {local_get2(["buy_block"]);}, 1000);  //failing, but why??
	wait_for_id();
    });
}
function wait_for_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for id");
    variable_get(["id"], new_channel);
}
function new_channel(id) {
    if (id == -1) {variable_get(["id"], new_channel);}
    else {
	console.log("new channel");
	console.log("id is ");
	console.log(id);
	local_get(["new_channel", [127,0,0,1], 3020, 1120000, 1100000, 50]);
	console.log("after new channel");
	address2(id);
    }
}
