var height = document.createElement("div");
height.id = "height";
document.body.appendChild(height);
variable_get(["height"], height2);
function height2(h) {
    console.log("height 2");
    var balance = document.getElementById("height");
    balance.innerHTML = "height ".concat((h).toString());
    console.log("height 3");
}
