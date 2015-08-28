-module(block_pointers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, get/3,put/3,test/0,height/1,append/2]).
-define(file, "block_pointers.db").
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(_, _From, D) -> {reply, 0, D}.
handle_cast({put, X, N, Word}, D) -> 
    true = size(X) == Word,
    {ok, File} = file:open(?file, [write, read, raw]),
    file:pwrite(File, N*Word, X),
    file:close(File),
    {noreply, D}.
put(X, N, Word) -> gen_server:cast(?MODULE, {put, X, N, Word}).
append(X, Word) -> put(X, height(Word), Word).
get(N, Many, Word) -> 
    {ok, File } = file:open(?file, [read, binary, raw]),
    {ok, X} = file:pread(File, N*Word, Many*Word),
    file:close(File),
    X.
height(Word) -> filelib:file_size(?file) div Word.
test() -> 
    A = round(math:pow(2, 48)) - 1,
    C = <<A:48>>,
    B = <<"ABCD">>,
    X = << B/binary, C/binary >>,
    put(X, 2, 10),
    put(<<"          ">>, 1, 10),
    put(<<"asdfghjkl;">>, 0, 10),
    timer:sleep(200),
    get(0, 3, 10).

