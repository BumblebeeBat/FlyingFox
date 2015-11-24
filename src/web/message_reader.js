setTimeout(get_message, 5000);
function get_message() {
    console.log("get message");
    local_get(["get_msg", [127,0,0,1], 3020]);
    setTimeout(get_message, 4000);
}
