talk1();
function talk1() {
    var talk_button = document.createElement("BUTTON");
    talk_button.id = "talk_button";
    var talk_text = document.createTextNode("send message");
    talk_button.appendChild(talk_text);
    talk_button.onclick = talk_func;
    document.body.appendChild(talk_button);
    
    var talk_address = document.createElement("input");
    talk_address.id = "talk_address";
    talk_address.setAttribute("type", "text");
    var address_info = document.createElement("h8");
    address_info.innerHTML = "talk at: ";
    document.body.appendChild(address_info);
    document.body.appendChild(talk_address);
    variable_get(["id"], talk2);
}
function talk2(id) {
    talk_address = document.getElementById("talk_address");
    talk_address.value = id;
    document.body.appendChild(document.createElement("br"));

    var talk_words = document.createElement("textarea");
    talk_words.id = "talk_words";
    talk_words.cols = 40;
    talk_words.rows = 3;
    talk_words.style = "overflow:auto;";
    document.body.appendChild(talk_words);
}
function talk_func() {
    console.log("talk func");
    var to = document.getElementById("talk_address");
    var to2 = parseInt(to.value, 10);
    var msg = document.getElementById("talk_words");
    local_get(["send_msg", [127,0,0,1], 3020, to2, btoa(msg.value), 6]);//they have 6 seconds to read the messages.
    
}
