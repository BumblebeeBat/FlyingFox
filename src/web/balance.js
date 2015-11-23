var balance = document.createElement("div");
balance.id = "balance";
document.body.appendChild(balance);
var channel_balance = document.createElement("div");
channel_balance.id = "channel_balance";
document.body.appendChild(channel_balance);
var b_button = document.createElement("BUTTON");
b_button.id = "balance_button";
var button_text_node = document.createTextNode("update balance");
b_button.appendChild(button_text_node);
b_button.onclick = update;
document.body.appendChild(b_button);

function update() {
    console.log("update balance");
    variable_get(["balance"], update2);
}
function update2(bal) {
    console.log("update 2");
    console.log(bal);
    var balance = document.getElementById("balance");
    var b = (bal).toString();
    balance.innerHTML = "your balance ".concat(b);
    variable_get(["channel_balance", [127,0,0,1], 3020], update3);
}
function update3(channel_balance) {
    var balance = document.getElementById("channel_balance");
    balance.innerHTML = "channel balance ".concat((channel_balance).toString());
    
}
