%this blocklist will only be used when 2/3 of validators signed on an invalid block, which causes them to lose their deposit. 
-module(block_blacklist).
-export([append/2, read/0]).
-define(file, "blacklist.db").

read() -> lists:map(fun(X) -> element(2, X) end, db:read(?file)).
append(BlockHash, Height) -> db:save(?file, [{BlockHash, Height}|db:read(?file)]).
remove(BlockHash) -> db:save(?file, remove(BlockHash, db:read(?file))).
remove(BH, BL) when BH == element(1, hd(BL)) -> tl(BL);
remove(BH, BL) -> [hd(BL)|remove(BH, tl(BL))].
remove_old(Height) -> db:save(?file, remove_old(db:read(?file), Height - constants:finality())).
remove_old(BL, N) when BL == [] -> [];
remove_old(BL, N) when element(2, hd(BL)) < N ->
    remove_old(tl(BL), N);
remove_old(BL, N) -> [hd(BL)|remove_old(tl(BL), N)].

    
