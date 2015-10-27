function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

local_get([-6, "test"], function () {
    local_get2([-6, "sync", {127,0,0,1}, 3000]);
});
