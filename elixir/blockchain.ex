defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: [], hash: ""]]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def create_sign do
    {pub, priv}=Local.address
    acc = KV.get(pub)
    prev = KV.get("height")
    tot_bonds = KV.get("tot_bonds")
    prev = prev-1
    prev = KV.get(prev)
    w=Enum.filter(0..Constants.chances_per_address, fn(x) -> VerifyTx.winner?(acc[:bond], tot_bonds, VerifyTx.rng, pub, x) end) 
    ran=:crypto.rand_bytes(10)
    h=KV.get("height")+1
    KV.put("secret #{inspect h}", ran)
    secret=DetHash.doit(ran)
    tx=[type: "sign", prev_hash: prev[:hash], winners: w, secret_hash: secret]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)#####
    w
  end
  def create_reveal do
    {pub, priv}=Local.address
    h=KV.get("height")-Constants.epoch
    cond do
      h<1 -> nil
      true ->
        old_block=BlockchainPure.get_block(h)
        old_tx = old_block[:txs] |> Enum.filter(&(&1[:data][:type]=="sign")) |> Enum.filter(&(&1[:pub]==pub)) |> hd
        w=old_tx[:data][:winners]
        bond_size=old_block[:bond_size]
        tx=[type: "reveal", signed_on: h, winners: w, amount: length(w)*bond_size, secret: KV.get("secret #{inspect h}")]
        tx=Sign.sign_tx(tx, pub, priv)
        Mempool.add_tx(tx)
    end
  end
  def absorb(b) do
    add_block(b)
    create_sign
    create_reveal
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
    create_sign
    create_reveal
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
