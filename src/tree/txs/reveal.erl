-module(reveal).
-export([doit/7, reveal/0, origin_tx/2]).
-record(reveal_tx, {acc = 0, nonce = 0, secret = [], height = 0}).

reveal() ->
    Id = keys:id(),
    CurrentHeight = block_tree:height(),
    reveal2(Id, CurrentHeight - constants:max_reveal(), CurrentHeight - constants:min_reveal()).
reveal2(_, Start, End) when Start > End -> ok;
reveal2(Id, Start, End) ->%This is an inefficient implementation. Checks all 9 * finality() spots every time.
    if
	Start < 1 -> ok;
	true ->
	    case block_tree:read_int(Start) of
		none -> 
		    io:fwrite("no such block\n "),
		    io:fwrite(integer_to_list(Start)),
		    before_finality;
		OriginBlock ->
		    Block = sign:data(OriginBlock),
		    OriginTxs = block_tree:block2txs(Block),
		    case origin_tx(OriginTxs, Id) of
			none -> 
			    io:fwrite("did not sign\n"),
			    did_not_sign;
			X ->
			    SH = sign_tx:secret_hash(X),
			    BTS = block_tree:secret(Start-1, SH, block_tree:read(top), tx_pool:secrets()),
			    Secret = secrets:read(SH),
			    if
				Secret == none ->
				    io:fwrite("lost the sign\n"),
				    lost_the_secret;
				not BTS ->
				    already_did_it;
				true ->
				    tx_pool:absorb(keys:sign(#reveal_tx{acc = Id, nonce = accounts:nonce(block_tree:account(Id)) + 1, secret = Secret, height = Start}))
			    end
		    end
	    end
    end,
    reveal2(Id, Start + 1, End).

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, Secrets, NewHeight) ->    
    H = Tx#reveal_tx.height,
    true = H > 0,
    Hgap = NewHeight - H,
    true = Hgap > constants:min_reveal(),
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H, ParentKey),
    OriginTxs = block_tree:block2txs(sign:data(OriginBlock)),
    OriginTx = origin_tx(OriginTxs, Tx#reveal_tx.acc),
    WL = sign_tx:winners_length(OriginTx),
    SH = sign_tx:secret_hash(OriginTx),
    SH = hash:doit(Tx#reveal_tx.secret),
    Number = sign_tx:number(OriginTx),
    Secret = block_tree:secret(Number, SH, ParentKey, Secrets),
    Secret = true,
    %PH = sign_tx:prev_hash(OriginTx),
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    Power = block_tree:power(block_tree:block(ParentKey)),
    DReward = fractions:multiply_int(constants:delegation_reward(), Power) div constants:validators_elected_per_block(),
    %the other 2/3 of the block creator's fee, and account fees and money that get deleted in channels does not go to validators. Instead it is premanently deleted.
    TReward = (Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL,
    Acc = block_tree:account(Tx#reveal_tx.acc, ParentKey, Accounts),
    N = accounts:update(Acc, NewHeight, TReward, 0, 1, TotalCoins),
    Nonce = accounts:nonce(N),
    Nonce = Tx#reveal_tx.nonce,
    NewAccounts = dict:store(Tx#reveal_tx.acc, N, Accounts),
    NewSecrets = dict:store({Number, SH}, false, Secrets),
    %newsecret shouldn't use newheight, it should point to the block that was signed on.
    {Channels, NewAccounts, TotalCoins + TReward, NewSecrets}.

origin_tx([], _) -> none;
origin_tx([SignedTx|Txs], Acc) ->
    Tx = sign:data(SignedTx),
    E = element(1, Tx),
    if
	(E == sign_tx) ->
	    SA = sign_tx:acc(Tx),
	    if
		(SA == Acc) -> Tx;
		true -> origin_tx(Txs, Acc)
	    end;
	true -> origin_tx(Txs, Acc)
    end.
    
