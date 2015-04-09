defmodule BlockchainPure do
  def get_block(h) do
    KV.get(to_string(h))[:data]
  end
  def txs_filter(txs, type) do 
    Enum.filter(txs, fn(t) -> t[:data][:type]==type end)
  end
  def num_signers(txs) do 
    txs_filter(txs, "sign")
    |> Enum.map(fn(t) -> length(t[:data][:winners]) end) 
    |> Enum.reduce(0, &(&1+&2))
  end
  def being_spent(txs) do
    send_txs = txs_filter(txs, "spend")
    send_txs=Enum.map(send_txs, fn(t) -> t[:tx][:amount] end)
    send_txs=Enum.reduce(send_txs, 0, &(&1+&2))
  end
  def valid_block?(block) do
    #IO.puts("valid block? #{inspect block}")
    h=KV.get("height")
    h2=block[:data][:height]
    cond do
      not is_list(block) -> 
        IO.puts("block should be a dict")
        false
      block[:data][:height] != h+1 ->
        IO.puts("incorrect height")
        false
      not Sign.verify_tx(block) -> 
        IO.puts("bad signature #{inspect block}")
        false
      true -> valid_block_2?(block, h)
    end
  end
  def valid_block_2?(block, h) do
    txs=block[:data][:txs]
    #true=VerifyTx.check_txs(txs)
    #run more checks
    #safety deposit per signer must be the same for each.
    #make sure block_hash matches previous block.
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    #make sure it has enough signers.
    sign_txs=txs_filter(txs, "sign")
    spending=being_spent(txs)
    signers = Enum.map(sign_txs, fn(t) -> t[:pub] end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
    ns=num_signers(sign_txs)
    cond do
      not VerifyTx.check_txs(txs) ->
           IO.puts("invalid tx")
           false
      ns <= Constants.signers_per_block*2/3 -> 
        IO.puts("not enough signers")
        false
      true -> valid_block_3?(block, ns, balances, spending)
    end
  end
  def valid_block_3?(block, ns, balances, spending) do
    {p, q}=Local.address
    signer_bond=block[:data][:bond_size]/ns
    sb = Enum.reduce(balances, nil, &(min(&1, &2)))
    txs = block[:data][:txs] 
    bs=block[:data][:bond_size]
    txs = Enum.filter(txs, fn(t) -> t[:data][:type]=="sign" end)
    cond do
      sb < signer_bond -> 
        IO.puts("poorest signer can afford")
        false
      bs<spending*3 -> 
        IO.puts("not enough bonds to spend that much")
        false
      block[:meta][:revealed] != [] -> 
        IO.puts("none revealed? weird error")
        false
      true -> true
    end
  end
  def blockhash(height, txs) do
    prev_block=KV.get(height-1)
    DetHash.doit([prev_block[:hash], txs])
  end
  def buy_block do
    height=KV.get("height")
    {pub, priv}=Local.address
    txs=Mempool.txs
    bh=blockhash(height, txs)
    new=[height: height+1, txs: txs, hash: bh, bond_size: 100_000]
    new=Sign.sign_tx(new, pub, priv)
    Dict.put(new, :meta, [revealed: []])
  end
end
