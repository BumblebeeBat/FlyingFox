%this module keeps track of how the accounts looked at the block that just reached finality. 
%When looking up an accounts status, you first need to check the blocktree to see if that account was edited in any of the blocks in recent history.
-module(finality_accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,write/3,test/0,size/0]).
-define(file, "accounts.db").
%Pub is 65 bytes. balance is 48 bits. bringing the total to 71 bytes.
-define(word, 71).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, N, Val}, D) -> 
    false = N > size(),
    {ok, File} = file:open(?file, [write, read, raw]),
    file:pwrite(File, N*?word, Val),
    file:close(File),
    {noreply, D}.
handle_call({read, N}, _From, D) -> 
    {ok, File } = file:open(?file, [read, binary, raw]),
    {ok, X} = file:pread(File, N*?word, ?word),
    file:close(File),
    {reply, X, D}.
read(N) -> 
    Val = gen_server:call(?MODULE, {read, N}),
    <<Balance:48, P/binary>> = Val,
    Pub = base64:encode(P),
    {Pub, Balance}.
write(N, Pub, Balance) -> 
    P = base64:decode(Pub),
    65 = size(P),
    Val = <<Balance:48, P/binary>>,
    gen_server:cast(?MODULE, {write, N, Val}).
size() -> filelib:file_size(?file) div ?word.
append(Pub, Balance) -> write(size(), Pub, Balance).
test() -> 
    Pub = <<"BIXotG1x5BhwxVKxjsCyrgJASovEJ5Yk/PszEdIoS/nKRNRv0P0E8RvNloMnBrFnggjV/F7pso/2PA4JDd6WQCE=">>,
    Balance = 50000000,
    append(Pub, Balance),
    {Pub, Balance} = read(0),
    success.
