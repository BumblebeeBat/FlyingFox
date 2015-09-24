%the hard drive stores {f, pubkey, encrypted(privkey), encrypted("sanity")).
%the ram stores either {pubkey, privkey} or {pubkey, ""} depending on if this node is locked.
-module(keys).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, pubkey/0,sign/1,sign/2,raw_sign/1,load/3,unlock/1,lock/0,status/0,change_password/2,new/1,shared_secret/1]).
-define(LOC(), "keys.db").
-define(SANE(), <<"sanity">>).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("keys died"), ok.
-record(f, {pub = "", priv = "", sanity = ""}).
init(ok) -> 
    X = db:read(?LOC()),
    if
        X == "" -> 
            K = #f{},%we should probably make a new key instead.
            db:save(?LOC(),K);
        true -> K = #f{pub=X#f.pub}
    end,
    {ok, K}.
store(Pub, Priv, Brainwallet) -> 
    X = #f{pub=Pub, priv=encryption:bin_enc(Brainwallet, Priv), sanity=encryption:bin_enc(Brainwallet, ?SANE())},
    db:save(?LOC(), X),
    Y = db:read(?LOC()),
    Y = X.
handle_call({ss, Pub}, _From, R) ->
    {reply, sign:shared_secret(Pub, R#f.priv), R};
handle_call({raw_sign, _}, _From, R) when R#f.priv=="" ->
    {reply, "need to unlock passphrase", R};
handle_call({raw_sign, M}, _From, X) when not is_binary(M) ->
    {reply, "not binary", X};
handle_call({raw_sign, M}, _From, R) ->
    {reply, sign:sign(M, R#f.priv), R};
handle_call({sign, M, Accounts}, _From, R) -> 
    {reply, sign:sign_tx(M, R#f.pub, R#f.priv, Accounts), R};
handle_call(status, _From, R) ->
    Y = db:read(?LOC()),
    Out = if
              Y#f.priv == "" -> empty;
              R#f.priv == "" -> locked;
              true -> unlocked
          end,
    {reply, Out, R};
handle_call(pubkey, _From, R) -> {reply, R#f.pub, R}.
handle_cast({load, Pub, Priv, Brainwallet}, _R) ->
    store(Pub, Priv, Brainwallet),
    {noreply, #f{pub=Pub, priv=Priv}};
handle_cast({new, Brainwallet}, _R) ->
    {Pub, Priv} = sign:new_key(),
    store(Pub, Priv, Brainwallet),
    {noreply, #f{pub=Pub, priv=Priv}};
handle_cast({unlock, Brainwallet}, _) ->
    X = db:read(?LOC()),
    ?SANE() = encryption:bin_dec(Brainwallet, X#f.sanity),
    Priv = encryption:bin_dec(Brainwallet, X#f.priv),%err
    {noreply, #f{pub=X#f.pub, priv=Priv}};
handle_cast(lock, R) -> {noreply, #f{pub=R#f.pub}};
handle_cast({change_password, Current, New}, R) ->
    X = db:read(?LOC()),
    ?SANE() = encryption:bin_dec(Current, X#f.sanity),
    Priv = encryption:bin_dec(Current, X#f.priv),
    store(R#f.pub, Priv, New),
    {noreply, R};
handle_cast(_, X) -> {noreply, X}.
handle_info(_, X) -> {noreply, X}.
pubkey() -> gen_server:call(?MODULE, pubkey).
sign(M) -> gen_server:call(?MODULE, {sign, M, dict:new()}).
sign(M, Accounts) -> gen_server:call(?MODULE, {sign, M, Accounts}).
raw_sign(M) -> gen_server:call(?MODULE, {raw_sign, M}).
load(Pub, Priv, Brainwallet) -> gen_server:cast(?MODULE, {load, Pub, Priv, Brainwallet}).
unlock(Brainwallet) -> gen_server:cast(?MODULE, {unlock, Brainwallet}).
lock() -> gen_server:cast(?MODULE, lock).
status() -> gen_server:call(?MODULE, status).
change_password(Current, New) -> gen_server:cast(?MODULE, {change_password, Current, New}).
new(Brainwallet) -> gen_server:cast(?MODULE, {new, Brainwallet}).
shared_secret(Pub) -> gen_server:call(?MODULE, {ss, Pub}).
