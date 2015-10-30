-module(download_blocks).
-export([sync/2]).

%download_blocks:sync({127,0,0,1}, 3020).
sync(IP, Port) ->
    {ok, PeerData} = talker:talk({height}, IP, Port),
    TheirHeight = PeerData,
    MyHeight = block_tree:height(),
    MH = MyHeight + constants:max_reveal(),
    if
        TheirHeight > MH ->
            fresh_sync(IP, Port, PeerData);
        TheirHeight > MyHeight ->
            get_blocks(MyHeight + 1, TheirHeight, IP, Port),
	    get_txs(IP, Port);
        true -> 0
    end.

get_starter_block(IP, Port, Height) ->
    %keep walking backward till we get to a block that has a backup hash...
    Z = block_tree:backup(Height),
    if
	Z -> talker:talk({block, Height}, IP, Port);
	true -> get_starter_block(IP, Port, Height - 1)
    end.
	     
absorb_stuff(Files, IP, _Port) ->
    %should download files.
    io:fwrite("absorb stuff").

fresh_sync(IP, Port, PeerData) ->
    io:fwrite("fresh sync"),
    TheirHeight = PeerData,
    Z = fractions:multiply_int(constants:backup(), constants:max_reveal()),
    MyHeight = block_tree:height(),
    if 
	TheirHeight < Z -> 
	    get_blocks(MyHeight + 1, TheirHeight, IP, Port);
	true ->
	    SignedBlock = get_starter_block(IP, Port, TheirHeight),
	    Block = sign:data(SignedBlock),
	    block_finality:append(SignedBlock, block_tree:block_number(Block)),
	    DBRoot = block_tree:block_root(Block),
	    absorb_stuff(backup:backup_files(), IP, Port),
	    DBRoot = backup:hash(),
	    get_blocks(MyHeight + 1, TheirHeight, IP, Port)
    end,
    0.
    %starting from recent block, walk backward to find the backup hash.
    %download the files, and check that they match the backup hash.
    %load the blocks in from oldest to newest.

get_blocks(Start, Finish, IP, Port) when Start>Finish -> ok;
get_blocks(Start, Finish, IP, Port) ->
    {ok, SignedBlock} = talker:talk({block, Start}, IP, Port),
    block_tree:absorb([SignedBlock]),
    get_blocks(Start + 1, Finish, IP, Port),
    ok.
absorb_txs([]) -> ok;
absorb_txs([Tx|T]) -> 
    spawn(tx_pool, absorb, [Tx]),
    timer:sleep(100),
    absorb_txs(T).
get_txs(IP, Port) ->
    {ok, Txs} = talker:talk({txs}, IP, Port),
    io:fwrite(packer:pack(Txs)),
    absorb_txs(Txs ++ Txs),
    ok.
