function register_doit() {
    local_get(["sync", IP, Port]);
    console.log("wait for id");
    variable_get(["id"], new_channel);
}
function new_channel(id) {
    if (id == -1) {variable_get(["id"], new_channel);}
    else {
	console.log("new channel");
	console.log("id is ");
	console.log(id);
	local_get(["new_channel", IP, Port, 1120000, 1100000, 50]);
	console.log("after new channel");
	address2(id);
    }
}
