%should rely on blocktree_kv modules to hold all the data, so even if we crash, we don't lose anything. it should store blocks by hash, and should store lists of block_hashes by height.
%the data being all the blocks since the epoch in ram under the control of this module. Older blocks go to the block_finality module.
%this module has no data, it's job is to listen for goto commands to attempt walks around the blocktree. 

%we need to add this. If there are 3 or more blocks at the same height, we need to blacklist that height, and refuse any blocks for that height.
-module(blocktree).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1,account/1,test/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

handle_cast({absorb, Blocks}, X) -> 
    absorb2(Blocks),
    {noreply, X}.
-record(signed, {data="", sig="", sig2="", revealed=[]}).
new_tree() ->
    Genesis = #block{},
    _Ghash = hash:doit(Genesis).
    %write(0, [Ghash]),
    %write(Ghash, #signed{data=Genesis}),
    %write(<<"height">>, 0).
init(ok) -> 
%check if we are recovering from a block that caused a crash and is now blacklisted. See if you can use the block to slash all the validators who signed it. Goto the highest non-blacklisted block in the tree.
{ok, []}.
back() -> 0.%opposite of forward, keep the digest memoized.
contains(L, _X) when L == [] -> false;
contains(L, X) when hd(L) == X -> true;
contains(L, X) -> contains(tl(L), X).
forward(Block) -> 
    %if we already calculated the digest for this block before, then don't do it again, and don't do the blacklist stuff.
    BL = block_blacklist:read(),
    BH = hash:doit(Block#signed.data),
    BL = block_blacklist:read(),
    false = contains(BL, BH),
    block_blacklist:append(BH),
    Height = Block#signed.data#block.height,
%reads txs to make a digest of state-changes for this block.
%give out rewards for validators in the digest.
%take fee from block creator in the digest.
%does time-saving checks to make sure the block is valid.
%besides all the digests for each block, we need a digest pointer that keeps track of the most recent update for each key between our current block, and the top of the finality chain. That way we can see if a key is in the digest in a single lookup. NO THIS IS A BAD IDEA. 10 lookups in 10 small dictionaries is faster than 1 lookup in the mega-dictionary made from combining them.
%does a detailed check to make sure the block is valid.
%add the block and digest to their modules.
%It is probably easier to do validity checks AFTER computing the digest. It is easy to make sure all balances are positive by looking at the digest.
%dump the mempool.
%if we are above height epoch, we may need to prune one or more blocks from the blocktree_kv.
    block_blacklist:remove(BH),
    block_blacklist:remove_old(Height),
    %the block and digest should be saved to a file so if blocktree crashes, we don't have to re-compute any digests.
    0.
goto(_BlockId) ->  
%don't go back in time.
%back up to shared history, then walk forward to the goal.
    0.
absorb4(Signed_block) ->
    Block = Signed_block#signed.data,
    true = is_record(Block, block),
    Size = size(packer:pack(Block)),
    true = Block#block.bond_size > constants:consensus_byte_price() * Size,
    %H = read(<<"height">>),
    Parentt = 0,%read(Block#block.hash),
    Parent = Parentt#signed.data,
    B = Block#block.height - 1,
    A = Parent#block.height, 
    A = B,
%Slasher needs to be general enough to punish validators for signing on anything that could get accepted by this function.
%use hashmath to make sure validators are valid.
%only add blocks that 
%were validated by enough signers,
%can be used to slash those signers, if they double-signed.
%if it passes all the tests, add it to the blocks kv.
%if it is heigher than our current height, call goto on it.
0.    
%absorb3(Block) -> spawn(blocktree, absorb4, [Block]).
absorb3(Block) -> spawn(fun() -> absorb4(Block) end).
absorb2(Blocks) -> lists:map(fun(X) -> absorb3(X) end, Blocks).
%absorb(Blocks) -> spawn(blocktree, absorb2, [Blocks]).
absorb(Blocks) -> gen_server:cast(?MODULE, {absorb, Blocks}).

account(N) -> get_account(N, block_digests:keys()).
get_account(N, Keys) when Keys == [] -> finality_accounts:read(N);
get_account(N, Keys) ->
    Digest = block_digests:get(hd(Keys)),
    case dict:find(N, element(1, Digest)) of
	error -> get_account(N, tl(Keys));
	{ok, Val} -> Val
    end.
channel(N) -> get_channel(N, block_digests:keys()).
get_channel(N, Keys) when Keys == [] -> finality_channels:read(N);
get_channel(N, Keys) ->
    Digest = block_digests:get(hd(Keys)),
    case dict:find(N, element(2, Digest)) of
	error -> get_channel(N, tl(Keys));
	{ok, Val} -> Val
    end.

test() ->
    new_tree(),
    back(),
    contains(1,2),
    forward(1),
    channel(1),
    goto(1).
    
