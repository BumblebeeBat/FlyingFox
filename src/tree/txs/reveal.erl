-module(reveal).
-export([doit/6]).
-record(reveal_tx, {acc = 0, nonce = 0, secret = [], height = 0}).

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, NewHeight) ->
    H = Tx#reveal_tx.height,
    true = H > 1,
    Hgap = NewHeight - H,
    true = Hgap > constants:min_reveal(),
    true = Hgap < constants:max_reveal(),
    OriginBlock = block_tree:read_int(H),
    OriginTxs = block_tree:txs(OriginBlock),
    OriginTx = origin_tx(OriginTxs, Tx#reveal_tx.acc),%parent key should be an input!!!
    WL = sign_tx:winners_length(OriginTx),
    Reward = constants:portion_of_block_creation_fee_validators() div constants:maximum_validators_per_block(),
    Power = block_tree:power(block_tree:block(ParentKey)),
    DReward = fractions:multiply_int(constants:delegation_reward(), Power) div constants:maximum_validators_per_block(),
    %all the delegation fees go to validators pot.
    %the other 2/3 of the block creator's fee, and account fees and money that get deleted in channels does not go to validators. Instead it is premanently deleted.
    %and we need each block to say how much money is left.
    %Fees need to decrease over time to relate to the new money supply.
    N = accounts:update(Tx#reveal_tx.acc, NewHeight, ((Reward + DReward +  constants:security_bonds_per_winner()) * WL), 0, 1),
    NewAccounts = dict:store(Tx#reveal_tx.acc, N, Accounts),
    {Channels, NewAccounts, TotalCoins + ((DReward + Reward) * WL)}.%total_coins increase by dreward.

origin_tx([SignedTx|Txs], Acc) ->
    Tx = sign:data(SignedTx),
    E = element(1, Tx),
    SA = sign_tx:acc(Tx),
    if
	(E == sign_tx) and (SA == Acc) ->
	    Tx;
	true -> origin_tx(Txs, Acc)
    end.
    
