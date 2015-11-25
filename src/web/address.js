var address = document.createElement("div");
address.id = "address";
document.body.appendChild(address);

setTimeout(function () {variable_get(["new_pubkey", btoa("abc")], address1);}, 1000);
function address1(x) {
    console.log("address 1");
    register_doit();
}

function address2(id) {
    console.log("address 2");
    var balance = document.getElementById("address");
    balance.innerHTML = "address ".concat((id).toString());
    console.log("address 3");
}
