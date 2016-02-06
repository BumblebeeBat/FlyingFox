-module(download_blocks).
-export([sync/2, absorb_txs/1]).

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
            get_blocks(MyHeight + 1, TheirHeight, IP, Port);
        true -> 0
    end,
    get_txs(IP, Port).
    

get_starter_block(IP, Port, Height) ->
    %keep walking backward till we get to a block that has a backup hash...
    Z = block_tree:backup(Height),
    if
	Z -> talker:talk({block, Height}, IP, Port);
	Height < 0 -> io:fwrite("starter block failure"), 1=2;
	true -> get_starter_block(IP, Port, Height - 1)
    end.
	     
absorb_stuff([], IP, Port) -> ok;
absorb_stuff([File|T], IP, Port) ->
    %should download files.
    %File is an atom, which cannot be packed with packer...
    {ok, Size} = talker:talk({backup_size, File}, IP, Port),
    io:fwrite("absorb stuff\n"),
    io:fwrite(File),
    io:fwrite("\n"),
    {ok, F} = file:open(File, [binary, raw, write, read]),
    absorb2(File, F, 0, Size, IP, Port),
    file:close(F),
    file:copy(File, "backup/"++File),
    absorb_stuff(T, IP, Port). 
absorb2(_, _, Step, Size, _, _) when Step > Size -> ok;
absorb2(FileName, File, Step, Size, IP, Port) ->
    io:fwrite("size "),
    io:fwrite(integer_to_list(Size)),
    io:fwrite("\n"),
    io:fwrite("step "),
    io:fwrite(integer_to_list(Step)),
    io:fwrite("\n"),
    io:fwrite("port "),
    io:fwrite(integer_to_list(Port)),
    io:fwrite("\n"),
    io:fwrite("file name "),
    io:fwrite(FileName),
    io:fwrite("\n"),
    {ok, Chunk} = talker:talk({backup_read, FileName, Step}, IP, Port),
    file:pwrite(File, Step * constants:word_size(), Chunk),
    absorb2(FileName, File, Step + 1, Size, IP, Port).

fresh_sync(IP, Port, PeerData) ->
    TheirHeight = PeerData,
    Z = fractions:multiply_int(constants:backup(), constants:max_reveal()),
    MyHeight = block_tree:height(),
    if 
	TheirHeight < Z -> 
	    get_blocks(MyHeight + 1, TheirHeight, IP, Port);
	true ->
	    {ok, SignedBlock} = get_starter_block(IP, Port, TheirHeight),
	    %Is 246 or no??
	    io:fwrite(packer:pack(SignedBlock)),
	    Block = sign:data(SignedBlock),
	    N = block_tree:block_number(Block),
	    block_pointers:set_start(N),
	    block_finality:append(SignedBlock, block_tree:block_number(Block)),
	    DBRoot = block_tree:block_root(Block),
	    io:fwrite("fs 3"),
	    absorb_stuff(backup:files(), IP, Port),
	    DBRoot = backup:hash(),
	    io:fwrite("fs 32"),
	    {ok, StartBlock} = talker:talk({block, N - constants:finality() - 1}, IP, Port),
	    io:fwrite("fs 33"),
	    io:fwrite("startBlock "),
	    io:fwrite(packer:pack(StartBlock)),
	    io:fwrite("\n"),
	    block_tree:unsafe_write(StartBlock),
	    io:fwrite("fs 34"),
	    get_blocks(N - constants:finality(), N - 1, IP, Port),
	    io:fwrite("fs 35"),
	    block_tree:absorb([SignedBlock]),
	    %block_tree:unsafe_write(SignedBlock),%need from finality earlier.
	    io:fwrite("fs 4"),
	    io:fwrite("fs 4"),
	    get_blocks(N + 1, TheirHeight, IP, Port),
	    io:fwrite("fs 5")
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
    MyTxs = tx_pool:txs(),
    absorb_txs(Txs),
    Respond = set_minus(MyTxs, Txs),
    if
	length(Respond) > 0 ->
	    talker:talk({txs, Respond}, IP, Port);
	true -> ok
    end,
    ok.
set_minus(A, B) -> set_minus(A, B, []).
set_minus([], _, Out) -> Out;
set_minus([A|T], B, Out) ->
    C = is_in(A, B),
    if
	C -> set_minus(T, B, Out);
	true -> set_minus(T, B, [A|Out])
    end.
is_in(A, []) -> false;
is_in(A, [A|T]) -> true;
is_in(A, [B|T]) -> is_in(A, T).
    
	    

