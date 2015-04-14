defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: []]]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def buy_block(b) do
    a = add_block(b)
    if a do sign_reveal end
    a
  end
  def genesis_state do
    genesis_block
    a=Constants.empty_account
    ac=Constants.initial_coins
    b = ac/21
    a = Dict.put(a, :amount, 20*b)
    a = Dict.put(a, :bond, b)
    {creator_pub, _} = Keys.master_keys
    KV.put(creator_pub, a)
    KV.put("tot_bonds", b)
    sign_reveal
  end
  def remove_block do      
    h=KV.get("height")
    if h>0 do
        block=BlockchainPure.get_block(h)
        txs=block[:txs]
        n=BlockchainPure.num_signers(BlockchainPure.txs_filter(txs, "sign"))
        TxUpdate.txs_updates(txs, -1, round(block[:bond_size] / n))
        #give block creator his fee back.
        KV.put("height", h-1)
        Mempool.dump
        sign_reveal
        true 
    end
  end
  def add_block(block) do#walking through blocks, and adding blocks to the database should be different functions.
    cond do
      not BlockchainPure.valid_block?(block) -> 
        IO.puts("invalid block")
        false
      true ->
        h=KV.get("height")
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        #make sure it has enough signers.
        txs=block[:data][:txs]
        KV.put(to_string(h+1), block)
        n=BlockchainPure.num_signers(txs)
        TxUpdate.txs_updates(txs, 1, round(block[:data][:bond_size] / n))
        KV.put("height", h+1)
        Mempool.dump    
        true
    end
  end
end
