function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

var x = local_get([-6, "sync", [-6,127,0,0,1], 3020]);
console.log("tester");
