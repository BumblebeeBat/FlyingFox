-module(create_account_tx).
-export([doit/4]).
-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
-record(acc, {balance = 0, nonce = 0, pub = "", delegated = 0}).

next_top(DBroot, Accounts) -> next_top_helper(accounts:array(), accounts:top(), DBroot, Accounts).
next_top_helper(Array, Top, DBroot, Accounts) ->
    EmptyAcc = #acc{},
    case block_tree:account(Top, DBroot, Accounts) of
	EmptyAcc -> Top;
	_ ->
	    <<A:Top,_:1,B/bitstring>> = Array,
	    NewArray = <<A:Top,1:1,B/bitstring>>,
	    NewTop = accounts:walk(Top, NewArray),
	    next_top_helper(NewArray, NewTop, DBroot, Accounts)
    end.
    
doit(Tx, ParentKey, Channels, Accounts) ->
    F = block_tree:account(Tx#ca.from, ParentKey, Accounts),
    NewId = next_top(ParentKey, Accounts),
    true = NewId < constants:max_address(),
    NT = #acc{nonce = 0,
              pub = Tx#ca.pub,
              balance = Tx#ca.amount,
              delegated = 0},
    NF = #acc{nonce = F#acc.nonce + 1,
              pub = F#acc.pub,
              balance = F#acc.balance - Tx#ca.amount - constants:create_account_fee(),
              delegated = F#acc.delegated},
    Nonce = F#acc.nonce + 1,
    Nonce = Tx#ca.nonce,
    true = NT#acc.balance > 0,
    true = NF#acc.balance > 0,
    Accounts2 = dict:store(NewId, NT, Accounts),
    Accounts3 = dict:store(Tx#ca.from, NF, Accounts2),
    MyId = keys:id(),
    MyPub = keys:pubkey(),
    if
	((Tx#ca.pub == MyPub) and (MyId == -1)) -> keys:update_id(NewId);
	true -> 1 = 1
    end,
    {Channels, Accounts3}.

