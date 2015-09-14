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
handle_call(_, _Test, D) -> {reply, 0, D}.
handle_cast(Blocks, D) -> 
    io:fwrite("cast\n"),
    %io:fwrite(packer:pack(hd(Blocks))),
    lists:map(fun(X) -> write(X) end, Blocks),
    {noreply, D}.
write(SignedBlock) -> 
%Slasher needs to be general enough to punish validators for signing on anything that could get accepted by this function.
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = block_tree:is_key(BH),
    false = block_blacklist:is_key(BH),
    io:fwrite("write 1.5\n"),
    block_blacklist:append(BH, Block#block.height),
    io:fwrite("write 2\n"),
    block_tree:write(SignedBlock),%err
    block_blacklist:remove(BH),
    block_blacklist:remove_old(Block#block.height).
absorb(Blocks) -> 
    io:fwrite("absorb\n"),
    gen_server:cast(?MODULE, Blocks).
test() -> 
    io:fwrite("test\n"),
    Tx = {spend, 0},
    Txs = [keys:sign(Tx)],
    SignedParent = block_finality:top_block(),
    Parent = SignedParent#signed.data,
    PHash = hash:doit(Parent),
    Block = #block{txs = Txs, hash = PHash, height = 1},
    SignedBlock = #signed{data = Block},
    io:fwrite(packer:pack(SignedBlock)),
    absorb([SignedBlock]).
