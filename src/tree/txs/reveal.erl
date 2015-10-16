-module(reveal).
-export([doit/6, reveal/0]).
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
		    before_finality;
		OriginBlock ->
		    OriginTxs = block_tree:txs(OriginBlock),
		    case origin_tx(OriginTxs, Id) of
			none -> did_not_sign;
			X ->
			    SH = sign_tx:secret_hash(X),
			    case secrets:read(SH) of
				none -> lost_the_secret;
				Secret -> tx_pool:absorb(keys:sign(#reveal_tx{acc = Id, nonce = accounts:nonce(block_tree:account(Id)) + 1, secret = Secret, height = Start}))
			    end
		    end
	    end
    end,
    reveal2(Id, Start + 1, End).

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    H = Tx#reveal_tx.height,
    true = H > 1,
    Hgap = NewHeight - H,
    true = Hgap > constants:min_reveal(),
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H, ParentKey),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = origin_tx(OriginTxs, Tx#reveal_tx.acc),
    %make sure the revealed secret matches the secret hash from the originTx.
    %we need a new database of secret_hashes. It is like accounts, channels, and totalcoins.!!!
    WL = sign_tx:winners_length(OriginTx),
    Reward = fractions:multiply_int(constants:portion_of_block_creation_fee_validators(), TotalCoins),
    Power = block_tree:power(block_tree:block(ParentKey)),
    DReward = fractions:multiply_int(constants:delegation_reward(), Power) div constants:maximum_validators_per_block(),
    %the other 2/3 of the block creator's fee, and account fees and money that get deleted in channels does not go to validators. Instead it is premanently deleted.
    N = accounts:update(Tx#reveal_tx.acc, NewHeight, ((Reward + DReward + fractions:multiply_int(constants:security_bonds_per_winner(), TotalCoins)) * WL), 0, 1, TotalCoins),
    NewAccounts = dict:store(Tx#reveal_tx.acc, N, Accounts),
    {Channels, NewAccounts, TotalCoins + ((DReward + Reward) * WL)}.

origin_tx([], _) -> none;
origin_tx([SignedTx|Txs], Acc) ->
    Tx = sign:data(SignedTx),
    E = element(1, Tx),
    SA = sign_tx:acc(Tx),
    if
	(E == sign_tx) and (SA == Acc) ->
	    Tx;
	true -> origin_tx(Txs, Acc)
    end.
    
