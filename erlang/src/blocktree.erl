%should rely on a kv modules to hold all the data, so even if we crash, we don't lose anything. it should store blocks by hash, and should store lists of block_hashes by height.
%the data being all the blocks since the epoch in ram under the control of this module. Older blocks go to the blocks_db module.
%this module has no data, it's job is to listen for goto commands to attempt walks around the blocktree.

-module(blocktree).
-record(block, {height = 0, txs = [], hash = "", bond_size = 1000000, pub = ""}).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,goto/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(_, _From, X) -> {reply, X, X}.

handle_cast({goto, _BlockId}, X) -> 
%don't go back in time.
%back up to shared history, then walk forward to the goal.
{noreply, X}.
init(ok) ->
    %walk through all the blocks from block_db.
    %walk to one below the current top block based on what the kv says.
    %check if we crashed while checking/loading a block from the blocktree, if so, then it blacklists the block so we wont walk on it any more.
    0.
back() -> 0.%opposite of forward.
forward(_Block) -> 
%stores the blockhash, so that if we crash while checking or loading this block, it can be blacklisted in the kv, and we wont try checking or loading it again.
%does more detailed check to make sure the block is valid.
%reads txs to update state.
%give out rewards for block.
%take fee from block creator.
%dump the mempool.
    0.
goto(BlockId) ->  gen_server:cast(?MODULE, {goto, BlockId}).
absorb4(Block) ->
%only add blocks that 
%have minimum bond for their size,
%were validated by enough signers,
%can be used to slash those signers, if they double-signed.
%points to a valid parent block.    
%if it passes all the tests, add it to the blocks kv.
%if it is heigher than our current height, call goto on it.
0.    
absorb3(Block) -> spawn(blocktree, absorb4, [Block]).
absorb2(Blocks) -> lists:map(absorb3, Blocks).
absorb(Blocks) -> spawn(blocktree, absorb2, [Blocks]).

