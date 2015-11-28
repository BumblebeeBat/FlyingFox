sync1();
function sync1() {
    document.body.appendChild(document.createElement("br"));
    var button = document.createElement("BUTTON");
    button.id = "sync_button";
    var button_text = document.createTextNode("sync");
    button.appendChild(button_text);
    button.onclick = function() {
	local_get(["sync", [127,0,0,1], 3020]);
    };
    document.body.appendChild(button);
}
