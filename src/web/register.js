//register_doit();
variable_get(["channel_keys"], register_doit());
function register_doit(x) {
    local_get(["sync", IP, Port]);
    console.log("wait for id");
    if (x == []) {
	variable_get(["id"], new_channel);
    }
}
function new_channel(id) {
    if (id == -1) {variable_get(["id"], new_channel);}
    else {
	variable_get(["balance"], function(x) {new_channel2(id,x);})
    }
}
function new_channel2(id, bal) {
    C = Math.min(Math.floor(bal/2), 1000000);
    console.log("new channel");
    console.log("id is ");
    console.log(id);
    local_get(["new_channel", IP, Port, C, Math.floor(C/1.1), 50]);
    console.log("after new channel");
    address2(id);
}
