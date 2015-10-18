-module(all_secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,add/2,exists/2,remove/2]).
-define(LOC, "all_secrets.db").
-record(x, {start = 0, blocks = [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]}).%length of the empty lists is constants:max_reveal - constants:min_reveal + 2.
save(DB) -> db:save(?LOC, database2bytes(DB#x.start, DB#x.blocks)).
read() -> db:read(?LOC).
init(ok) -> 
    process_flag(trap_exit, true),
    X = read(),
    if
        X == "" -> 
            K = #x{},
	    save(K);
        true -> K = bytes2database(X)
    end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    save(K),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
remove_sh(X, [X|T]) -> T;
remove_sh(X, [H|T]) -> [H|remove_sh(X, T)].
replace_height(0, [_|T], NewBlock) -> [NewBlock|T];
replace_height(N, [Block|T], NewBlock) -> [Block|replace_height(N-1, T, NewBlock)].
handle_cast({remove, Height, SH}, X) -> 
    H = Height - X#x.start,
    NewX = #x{start = X#x.start, blocks = replace_height(H, X#x.blocks, remove_sh(SH, nth(H, X#x.blocks)))},
    {noreply, NewX};
handle_cast({add, Height, SH}, X) -> 
    Gap = constants:max_reveal() - constants:min_reveal(),
    Start = X#x.start,
    H = Height-Start,
    NewStart = max(Height - Gap - X#x.start, Start),
    NewX = #x{start = NewStart, blocks = remove_front(NewStart - Start, replace_height(H, X#x.blocks, [SH|nth(H, X#x.blocks)]))},
    {noreply, NewX}.
remove_front(0, T) -> T;
remove_front(X, [_|T]) -> remove_front(X-1, T).
nth(0, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).
in_list(_, []) -> false;
in_list(X, [X|_]) -> true;
in_list(X, [_|T]) -> in_list(X, T).
handle_call({exists, Height, SH}, _From, X) -> 
    H = Height - X#x.start,
    Block = nth(H, X#x.blocks),
    O = in_list(SH, Block),
    {reply, O, X}.
exists(Height, SH) -> gen_server:call(?MODULE, {exists, Height, SH}).
add(Height, SH) -> 
    E = exists(Height, SH),
    if 
	E -> ok;
	true -> gen_server:cast(?MODULE, {add, Height, SH})
    end.
	    
remove(Height, SH) ->
    E = exists(Height, SH),
    if
	E -> gen_server:cast(?MODULE, {remove, Height, SH});
	true -> ok
    end.

%234 blocks between minreveal and maxreveal * 54 secrets per block * 32 bytes per secret = 404352 bytes = ~1/2 megabyte, so we can keep it in ram.
%once a secret gets revealed, we should remove it.
%This is like accounts and blocks. This module stores how it looked at finality. Recent changes are in the blocktree.
%we should garbage collect everything before maxreveal.

%db = {Start, List} %Start is the start from where garbage collection happened. List is about 234 long, one for each height between minreveal and maxreveal.
%List = [SecretList1, SecretList2, ...] 
%SecretList = [SecretHash, SecretHash, ...] this is the secrets from a single block.
secrets2bytes([]) -> <<>>; %<<1a, 1b, ... 1z>>
secrets2bytes([Secret|T]) ->
    S = secrets2bytes(T),
    <<Secret/binary, S/binary>>.
list2bytes([]) -> <<>>; %<<Size1, 1a, 1b, ... 1z, Size2, 2a, 2b, ...>>
list2bytes([Secrets|T]) -> 
    S = secrets2bytes(Secrets),
    L = list2bytes(T),
    A = length(Secrets),
    <<A:8, S/binary, L/binary>>.
database2bytes(Start, List) ->
    %<<Height:38, Size1, 1a, 1b, ... 1z, Size2, 2a, 2b, ...>>
    L = list2bytes(List),
    <<Start:38, L/binary>>.
bytes2secrets(<<>>) -> [];
bytes2secrets(<<S:256, T/binary>>) -> [<<S:256>>|bytes2secrets(T)];
bytes2secrets(X) -> io:fwrite(X).
bytes2list(<<>>) -> [];
bytes2list(<<L:8, X/binary>>) -> bytes2list2(L, X).
bytes2list2(L, X) ->
    M = 256 * L,
    <<BinarySecrets:M, Y/binary>> = X,
    [bytes2secrets(<<BinarySecrets:M>>)|bytes2list(Y)].
    
bytes2database(B) ->
    <<Start:38, X/binary>> = B,
    L = bytes2list(X),
    {Start, L}.

test() ->
    DB = [[hash:doit(1), hash:doit(2)],[hash:doit(3), hash:doit(4)]],
    S = 4,
    A = database2bytes(S, DB),
    {S, DB} = bytes2database(A),
    add(4, hash:doit(5)),
    add(6, hash:doit(5)),
    add(5, hash:doit(5)),
    add(6, hash:doit(6)),
    true = exists(4, hash:doit(5)),
    remove(6, hash:doit(5)),
    remove(6, hash:doit(5)),
    false = exists(6, hash:doit(5)),
    true = exists(4, hash:doit(5)),
    success.
