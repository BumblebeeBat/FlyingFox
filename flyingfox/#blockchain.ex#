defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: []]]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def sign_reveal do
    TxCreator.create_sign
    TxCreator.create_reveal
  end
  def buy_block(b) do
    a = add_block(b)
    if a do sign_reveal end
    a
  end
  def genesis_state do
    genesis_block
    a=Constants.empty_account
    bonds =                    100_000_000_000_000
    a = Dict.put(a, :amount, 2_000_000_000_000_000)
    a = Dict.put(a, :bond, bonds)     
    {creator_pub, creator_priv} = {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="}
    KV.put(creator_pub, a)
    KV.put("tot_bonds", bonds)
    sign_reveal
  end
  def remove_block do
    h=KV.get("height")
    block=BlockchainPure.get_block(h)
    txs=block[:data][:txs]
    n=BlockchainPure.num_signers(BlockchainPure.txs_filter(txs, "sign"))
    TxUpdate.txs_updates(txs, -1, div(block[:data][:bond_size], n))
    #give block creator his fee back.
    KV.put("height", h-1)
  end
  def add_block(block) do
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
        TxUpdate.txs_updates(txs, 1, div(block[:data][:bond_size],n))
        KV.put("height", h+1)
        Mempool.dump    
        true
    end
  end
end
