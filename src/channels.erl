-module(channels).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read_channel/1,write/2,test/0,size/0,write_helper/3,top/0,array/0,delete/1,walk/2]).
-define(file, "channels.db").
-define(empty, "d_channels.db").
-define(word, 9).%20+20+32 = 72 bits = 9 bytes
-record(channel, {tc = 0, creator = 0, timeout = 0}).
write_helper(N, Val, File) ->
%since we are reading it a bunch of changes at a time for each block, there should be a way to only open the file once, make all the changes, and then close it. 
    case file:open(File, [write, read, raw]) of
        {ok, F} ->
            file:pwrite(F, N, Val),
            file:close(F);
        {error, _Reason} ->
            write_helper(N, Val, File)
    end.
init(ok) -> 
    case file:read_file(?empty) of
        {error, enoent} -> 
            Top = 0,
            DeletedArray = << 0 >>,
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
walk_helper(<< 257:9, B/bitstring>>, Counter) -> walk_helper(B, Counter + 9);
walk_helper(<< 1:1, B/bitstring>>, Counter) -> walk_helper(B, Counter + 1);
walk_helper(<< 0:1, _B/bitstring>>, Counter) -> Counter.
handle_cast({delete, N}, {Top, Array}) -> 
    Byte = hd(binary_to_list(read_empty(N))),
    Remove = bnot round(math:pow(2, N rem 9)),
    NewByte = Byte band Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,0:1,B/bitstring>>,
    write_helper(N*?word, #channel{}, ?file),
    {noreply, {min(Top, N), NewArray}};
handle_cast({write, N, Val}, {Top, Array}) -> 
    S = size(),
    if
        N > S -> write_helper(N div 8, <<0>>, ?empty);
        true -> 0 = 0
    end,
    Byte = hd(binary_to_list(read_empty(N))),
    Remove = round(math:pow(2, N rem 8)),
    NewByte = Byte bor Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,1:1,B/bitstring>>,
    false = N > size(),
    write_helper(N*?word, Val, ?file),
    {noreply, {walk(Top, NewArray), NewArray}}.
handle_call(array, _From, {Top, Array}) -> {reply, Array, {Top, Array}};
handle_call(top, _From, {Top, Array}) -> {reply, Top, {Top, Array}}.
top() -> gen_server:call(?MODULE, top).
array() -> gen_server:call(?MODULE, array).
delete(N) -> gen_server:cast(?MODULE, {delete, N}).
read_empty(N) -> 
    {ok, File} = file:open(?empty, [read, binary, raw]),
    case file:pread(File, N div 8, 1) of
	eof -> write_helper(N div 8, <<0>>, ?empty),
	       read_empty(N);
	{ok, X} -> file:close(File), X
    end.
read_file(N) -> 
    {ok, File} = file:open(?file, [read, binary, raw]),
    case file:pread(File, N*?word, ?word) of
	eof -> write_helper(N*?word, <<0:72>>, ?file),% 600=8*?word.
	       read_file(N);
	{ok, X} -> file:close(File), X
    end.
read_channel(N) -> %maybe this should be a call too, that way we can use the ram to see if it is already deleted?
    T = top(),
    if
	N >= T -> #channel{};
	true ->
	    X = read_file(N),%if this is above the end of the file, then just return an account of all zeros.
	    <<Tc:20, Timeout:20, Creator:32>> = X,
	    #channel{tc = Tc, timeout = Timeout, creator = Creator}
	    
    end.
write(N, Ch) ->
    Val = << (Ch#channel.tc):20,
	     (Ch#channel.timeout):20,
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
    Tc = 2,
    Creator = 0,
    Timeout = 555,
    A = #channel{tc = Tc, creator = Creator, timeout = Timeout},
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
    Ch = read_channel(2),
    Ch = read_channel(1),
    delete(1),
    delete(0),
    0 = top(),
    append(A),
    1 = top(),
    append(A),
    3 = top(),
    Ch = read_channel(0),
    %Height = Ch#channel.height,
    Creator = Ch#channel.creator,
    %Nonce = Ch#channel.nonce,
    Timeout = Ch#channel.timeout,
    Tc = Ch#channel.tc,
    success.
