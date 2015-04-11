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
  def being_spent(txs) do txs |> txs_filter("spend") |> Enum.map(fn(t) -> t[:data][:amount] end) |> Enum.reduce(0, &(&1+&2)) end
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
      true ->
        valid_block_2?(block)
    end
  end
  def valid_block_2?(block) do
    winners = block[:data][:txs] |> txs_filter("sign") |> Enum.map(&(length(&1[:data][:winners]))) |> Enum.reduce(0, &(&1+&2))
    cond do
      winners < Constants.signers_per_block*2/3 -> 
        IO.puts("not enough signers")
        false
      not VerifyTx.check_txs(block[:data][:txs]) ->
        IO.puts("invalid tx")
        false
      true -> valid_block_3?(block, winners) 
    end
  end
  def valid_block_3?(block, ns) do
    txs = block[:data][:txs] 
    sign_txs=txs_filter(txs, "sign")
    signers = Enum.map(sign_txs, fn(t) -> t[:pub] end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
    IO.puts("address #{inspect Keys.address}")
    {p, q}=Keys.address
    #signer_bond=block[:data][:bond_size]/ns
    poorest_balance = Enum.reduce(balances, nil, &(min(&1, &2)))
    spending=being_spent(txs)
    bs=block[:data][:bond_size]
    txs = Enum.filter(txs, fn(t) -> t[:data][:type]=="sign" end)
    cond do
      poorest_balance < bs -> 
        IO.puts("poorest signer cant afford")
        false
      bs*ns<spending*3 -> 
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
    {pub, priv}=Keys.address
    txs=Mempool.txs#remove expensive txs until we can afford it.
    bh=blockhash(height, txs)
    new=[height: height+1, txs: txs, hash: bh, bond_size: 10_000_000_000_000/Constants.signers_per_block*3/2]
    new=Sign.sign_tx(new, pub, priv)
    Dict.put(new, :meta, [revealed: []])
  end
end
