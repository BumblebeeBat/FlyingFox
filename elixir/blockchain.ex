defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: [], hash: ""]]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def sign_reveal do
    IO.puts("SR")
    TxCreator.create_sign
    IO.puts("SR")
    TxCreator.create_reveal
    IO.puts("SR")
  end
  def absorb(b) do
    add_block(b)
    sign_reveal
  end
  def genesis_state do
    IO.puts("genesis state 0")
    genesis_block
    a=Constants.empty_account
    bonds =                    100_000_000_000_000
    a = Dict.put(a, :amount, 2_000_000_000_000_000)
    a = Dict.put(a, :bond, bonds)     
    IO.puts("genesis state 1")
    {creator_pub, creator_priv} = {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="}
    KV.put(creator_pub, a)
    KV.put("tot_bonds", bonds)
    IO.puts("genesis state 3")
    sign_reveal
    IO.puts("genesis state 4")
  end
  def remove_block do
    h=KV.get("height")
    block=BlockchainPure.get_block(h)
    n=BlockchainPure.num_signers(BlockchainPure.txs_filter(block[:txs], "sign"))
    TxUpdate.txs_updates(block[:txs], -1, div(block[:bond_size], n))
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
        h2=block[:data][:height]
        txs=block[:data][:txs]
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        #make sure it has enough signers.
        sign_txs=BlockchainPure.txs_filter(txs, "sign")
        spending=BlockchainPure.being_spent(txs)
        ns = BlockchainPure.num_signers(sign_txs)
        signers = Enum.map(sign_txs, &(&1[:pub]))
        accs = Enum.map(signers, &(KV.get(&1)))
        balances = Enum.map(accs, &(&1[:bond]))
        signer_bond=block[:data][:bond_size]/ns
        sb = Enum.reduce(balances, nil, &(min(&1, &2)))
        {p, q}=Local.address
        KV.put(to_string(h2), block)
        txs = block[:data][:txs] 
        txs = Enum.filter(txs, fn(t) -> t[:data][:type]=="sign" end)
        bs=block[:data][:bond_size]
        TxUpdate.txs_updates(block[:data][:txs], 1, signer_bond)
        KV.put("height", h2)
        Mempool.dump    
    end
  end
end
