%this module cannot be parallelized because we could end up adding two blocks at once, and the lower-height block ends up getting labled as "top".
-module(block_tester).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,absorb/1]).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-record(signed, {data="", sig="", sig2="", revealed=[]}).
init(ok) ->  {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tester died!"), ok.
handle_info(_, X) -> {noreply, X}.
contains([], _) -> false;
contains(L, X) when hd(L) == X -> true;
contains(L, X) -> contains(tl(L), X).
handle_call(_, _Test, D) -> {reply, 0, D}.
handle_cast({write, K, V}, D) -> {noreply, dict:store(K, V, D)}.
write(SignedBlock) -> 
%Slasher needs to be general enough to punish validators for signing on anything that could get accepted by this function.
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = contains(block_tree:keys(), BH),
    BL = block_blacklist:read(),
    false = contains(BL, BH),
    block_blacklist:append(BH),
    block_tree:write(SignedBlock),
    block_blacklist:remove(BH),
    block_blacklist:remove_old(Block#block.height).
absorb(Blocks) -> lists:map(fun(X) -> write(X) end, Blocks).
test() -> 
    Tx = [],
    Txs = [#signed{data = Tx}],
    absorb([#block{txs = Txs}]).
