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

%We can't check blocks simultaniously.
contains([], _) -> false;
contains(L, X) when hd(L) == X -> true;
contains(L, X) -> contains(tl(L), X).
handle_call(_, _Test, D) -> {reply, 0, D}.
handle_cast({write, K, V}, D) -> {noreply, dict:store(K, V, D)}.
write(SignedBlock) -> 
    Block = SignedBlock#signed.data,
    BH = hash:doit(Block),
    false = contains(block_tree:keys(), BH),
    BL = block_blacklist:read(),
    false = contains(BL, BH),
    block_blacklist:append(BH),
%Slasher needs to be general enough to punish validators for signing on anything that could get accepted by this function.
%only add blocks that 
%were validated by enough signers,
%check that the amount bonded is sufficiently big compared to the amount being spent.
    true = is_record(Block, block),
    Size = size(packer:pack(Block)),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    Parent = block_tree:get(Block#block.hash),
    Height = Parent#block.height + 1,
    Height = Block#block.height,
    Accounts_dict = dict:new(),
    Channels_dict = dict:new(),
    ParentKey = Block#block.hash,
    {AccountsDict, ChannelsDict} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new()),
%give out rewards for validators in the digest.
%take fee from block creator in the digest.
    %make sure there is no negative money
    Key = hash:doit(Block),
    block_tree:write(Key, AccountsDict, ChannelsDict, ParentKey, SignedBlock),
    %block_tree:write(Key, SignedBlock),
    block_blacklist:remove(BH),
    block_blacklist:remove_old(Height).

    

absorb(Blocks) -> lists:map(fun(X) -> write(X) end, Blocks).
test() -> absorb([#block{}]).
