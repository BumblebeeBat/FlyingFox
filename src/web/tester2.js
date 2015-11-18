function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
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
	local_get(["new_channel", [127,0,0,1], 3020, 10000, 11000, 50]);
	variable_get(["channel_keys"], channel_spend);
    }
}
function channel_spend(keys) {
    var partner = 1;
    var amount = 200;
    if (keys == [-6]) {variable_get(["channel_keys"], channel_spend);}
    else {
	local_get(["lightning_spend", [127,0,0,1], 3020, partner, p
    }
}
