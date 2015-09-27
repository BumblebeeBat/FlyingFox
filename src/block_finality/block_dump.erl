-module(block_dump).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/2,write/1,test/0]).
-define(file, "blocks.db").
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Location, Bytes}, _From, X) -> 
    {ok, File} = file:open(?file, [read, raw, binary]),
    {ok, Y} = file:pread(File, Location, Bytes),
    file:close(File),
    {reply, Y, X};
handle_call({write, Bytes}, _From, X) ->
    {ok, File} = file:open(?file, [read, write, raw]),
    Location = filelib:file_size(?file),
    file:pwrite(File, Location, Bytes),
    file:close(File),
    {reply, {Location, size(Bytes)}, X}.
read(Location, Bytes) -> zlib:uncompress(gen_server:call(?MODULE, {read, Location, Bytes})).
write(Bytes) -> gen_server:call(?MODULE, {write, zlib:compress(Bytes)}).

test() ->
    S = <<"1234567abcdef">>,
    {X, Y} = write(S),
    timer:sleep(10),
    {A, Y} = write(S),
    false = A == X,
    S = read(X, Y),
    S = read(A, Y),
    success.
