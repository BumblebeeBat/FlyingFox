%I need to change this. If accounts get deleted, we should re-use the space. top should point to the highest empty spot. If something lower than us gets deleted, move top down there. 
%After an append, walk top upward until you find an empty spot to point to.
%We _need_ databases to be identical accross nodes. 
%depending on how complicated it is to compute the next top, we may have to charge an additional fee when people delete in bad spots.


%Instead of RAM just holding top, it should also hold a bunch of bits. 
%Each bit coorisponds to an address in the database. 
%If the bit is set to zero, then that address is ready to be written in.
%Top should point to the lowest known address that is deleted.
%The byte array should be backed up to disk. Instead of writing the entire thing to disk at each block, we should manipulate individual bits in the file at each block. 
-module(finality_accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,write/2,test/0,size/0,write_helper/2,top/0]).
-define(file, "accounts.db").
-define(empty, "deleted_accounts.db").
%-define(zeros, << 0:103 >>).
-define(zeros, #acc{balance = 0, nonce = 0, pub = 0}).
%Pub is 65 bytes. balance is 48 bits. Nonce is 32 bits. bringing the total to 75 bytes.
%1 bit per account, to see if it was deleted.
-define(word, 75).
-record(acc, {balance = 0, nonce = 0, pub = ""}).
write_helper(N, Val) ->
%this is a really dumb way to do it.
%since we are reading it a bunch of changes at a time for each block, there should be a way to only open the file once, make all the changes, and then close it. 
    {ok, File} = file:open(?file, [write, read, raw]),
    file:pwrite(File, N*?word, Val),
    file:close(File).
init(ok) -> 
    B = filelib:is_regular(?file),
    if
        not B ->
            P = base64:decode(constants:master_pub()),
            Balance = constants:initial_coins(),
            %A = #acc{balance = Balance, nonce = 0, pub = P},
            write_helper(0, <<Balance:48, 0:32, P/binary>>),
            Top = 1,
            DeletedArray = << 1:1 >>;
            %create the DeletedArray with 1 bit set to 1.
        true -> 
            {ok, File } = file:open(?empty, [read, binary, raw]),
            {ok, DeletedArray} = file:read_file(File),
            file:close(File),
            Top = walk(0, DeletedArray),
            0 = 0
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
walk_helper(<< A:1, B/bitstring>>, Counter) -> 
    if
        A == 1 -> walk_helper(B, Counter + 1);
        true -> Counter
    end.
handle_cast({delete, N}, {Top, Array}) -> 
    %zero out the bit in ?empty
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,0:1,B>>,
    write_helper(N, ?zeros),
    {noreply, {min(Top, N), NewArray}};
handle_cast({write, N, Val}, {Top, Array}) -> 
    %update the bit in ?empty
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,1:1,B>>,
    false = N > size(),
    write_helper(N, Val),
    {noreply, {walk(Top, NewArray), NewArray}}.
%handle_call({read, N}, _From, D) -> 
%    {ok, File } = file:open(?file, [read, binary, raw]),
%    {ok, X} = file:pread(File, N*?word, ?word),
%    file:close(File),
%    {reply, X, D};
handle_call(top, _From, {Top, Array}) -> {reply, Top, {Top, Array}}.
top() -> gen_server:call(?MODULE, top).
delete(N) -> gen_server:cast(?MODULE, {delete, N}).
read(N) -> 
    {ok, File} = file:open(?file, [read, binary, raw]),
    {ok, X} = file:pread(File, N*?word, ?word),
    file:close(File),
    <<Balance:48, Nonce:32, P/binary>> = X,
    Pub = base64:encode(P),
    #acc{balance = Balance, nonce = Nonce, pub = Pub}.
%{Pub, Nonce, Balance}.
write(N, Acc) -> %Pub, Nonce, Balance) -> 
    P = base64:decode(Acc#acc.pub),
    65 = size(P),
    Val = << (Acc#acc.balance):48, 
             (Acc#acc.nonce):32, 
             P/binary >>,
    gen_server:cast(?MODULE, {write, N, Val}).
size() -> filelib:file_size(?file) div ?word.
append(Acc) -> write(top(), Acc).
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
    %accounts.db needs to be empty before starting node to run this test.
    %Pub = <<"BIXotG1x5BhwxVKxjsCyrgJASovEJ5Yk/PszEdIoS/nKRNRv0P0E8RvNloMnBrFnggjV/F7pso/2PA4JDd6WQCE=">>,
    %Balance = 50000000,
    %A = #acc{pub = Pub, nonce = 0, balance = Balance},
    %1 = top(),
    %append(A),
    %2 = top(),
    %append(A),
    %3 = top(),
    %delete(0),
    %0 = top(),
    %append(A),
    %3 = top(),
    %Acc = read(0),
    %Pub = Acc#acc.pub,
    %Balance = Acc#acc.balance,
    success.
