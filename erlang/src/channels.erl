-module(channels).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read_channel/1,write/2,test/0,size/0,write_helper/3,top/0,delete/1]).
-define(file, "channels.db").
-define(empty, "d_channels.db").
%20 bits for height. for creator... log(2, constants:max_address()) =~ at least 32 bits. The nonce needs to be at least as big as log(2, number of channels created per block). 12 bits should be fine. This limits us to creating 4096 channels per block at most.
-define(word, 8).%20+12+32 = 64 bits = 8 bytes
-record(channel, {height = 0, nonce = 0, creator = 0}).
write_helper(N, Val, File) ->
%since we are reading it a bunch of changes at a time for each block, there should be a way to only open the file once, make all the changes, and then close it. 
    case file:open(File, [write, read, raw]) of
        {ok, F} ->
            file:pwrite(F, N*?word, Val),
            file:close(F);
        {error, _Reason} ->
            write_helper(N, Val, File)
    end.
init(ok) -> 
    case file:read_file(?empty) of
        {error, enoent} -> 
            Top = 0,
            DeletedArray = << 0:8 >>,
            write_helper(0, DeletedArray, ?empty);
        {ok, DeletedArray} ->
            Top = walk(0, DeletedArray)
    end,
    {ok, {Top, DeletedArray}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
walk(Top, Array) -> 
    << _:Top, Tail/bitstring>> = Array,
    walk_helper(Tail, Top).
walk_helper(<<>>, Counter) -> Counter;
walk_helper(<< 127:8, B/bitstring>>, Counter) -> walk_helper(B, Counter + 8);
walk_helper(<< 1:1, B/bitstring>>, Counter) -> walk_helper(B, Counter + 1);
walk_helper(<< 0:1, _B/bitstring>>, Counter) -> Counter.
handle_cast({delete, N}, {Top, Array}) -> 
    Byte = hd(binary_to_list(read(N div 8, 1, ?empty))),
    Remove = bnot round(math:pow(2, N rem 8)),
    NewByte = Byte band Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,0:1,B/bitstring>>,
    write_helper(N, #channel{}, ?file),
    {noreply, {min(Top, N), NewArray}};
handle_cast({write, N, Val}, {Top, Array}) -> 
    S = size(),
    if
        N > S -> write_helper(N div 8, <<0>>, ?empty);
        true -> 0 = 0
    end,
    Byte = hd(binary_to_list(read(N div 8, 1, ?empty))),
    Remove = round(math:pow(2, N rem 8)),
    NewByte = Byte bor Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,1:1,B/bitstring>>,
    false = N > size(),
    write_helper(N, Val, ?file),
    {noreply, {walk(Top, NewArray), NewArray}}.
handle_call(top, _From, {Top, Array}) -> {reply, Top, {Top, Array}}.
top() -> gen_server:call(?MODULE, top).
delete(N) -> gen_server:cast(?MODULE, {delete, N}).
read(N, Bytes, F) -> 
    {ok, File} = file:open(F, [read, binary, raw]),
    {ok, X} = file:pread(File, N, Bytes),
    file:close(File),
    X.
read_channel(N) -> %maybe this should be a call too, that way we can use the ram to see if it is already deleted?
    T = top(),
    if
	N >= T -> #channel{};
	true ->
	    X = read(N*?word, ?word, ?file),%if this is above the end of the file, then just return an account of all zeros.
	    <<Height:20, Nonce:12, Creator:32>> = X,
	    #channel{height = Height, nonce = Nonce, creator = Creator}
	    
    end.
write(N, Ch) ->
    Val = << (Ch#channel.height):20,
	     (Ch#channel.nonce):12,
	     (Ch#channel.creator):32 >>,
    gen_server:cast(?MODULE, {write, N, Val}).
size() -> filelib:file_size(?file) div ?word.
append(Ch) -> write(top(), Ch).
test() -> 
    << 13:4 >> = << 1:1, 1:1, 0:1, 1:1 >>,%13=8+4+1
    0 = walk(0, << >>),
    0 = walk(0, << 0:1 >>),
    2 = walk(0, << 1:1, 1:1, 0:1, 1:1 >>),
    3 = walk(0, << 1:1, 1:1, 1:1, 0:1, 0:30 >>),
    1 = walk(0, << 2:2 >>),
    0 = walk(0, << 1:2 >>),
    2 = walk(0, << 24:5 >>),
    5 = walk(0, << 31:5 >>),
    5 = walk(2, << 31:5 >>),
    5 = walk(5, << 31:5 >>),
    %channels.db needs to be empty before starting node to run this test.
    Height = 2,
    Creator = 0,
    Nonce = 1837,
    A = #channel{height = Height, creator = Creator, nonce = Nonce},
    0 = top(),
    append(A),
    1 = top(),
    append(A),
    2 = top(),
    append(A),
    3 = top(),
    delete(0),
    0 = top(),
    append(A),
    3 = top(),
    delete(1),
    1 = top(),
    append(A),
    3 = top(),
    delete(1),
    delete(0),
    0 = top(),
    append(A),
    1 = top(),
    append(A),
    3 = top(),
    Ch = read_channel(0),
    Height = Ch#channel.height,
    Creator = Ch#channel.creator,
    Nonce = Ch#channel.nonce,
    success.