defmodule BlockchainPure do
  def get_block(h) do
    if is_integer(h) do h=KV.get(to_string(h)) end
    KV.get(h)
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
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    #IO.puts("valid block? #{inspect block}")
    h=KV.get("height")
    cond do
      not is_list(block) -> 
        IO.puts("block should be a dict #{inspect block}")
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
    #{p, q}=Keys.address
    #signer_bond=block[:data][:bond_size]/ns
    poorest_balance = Enum.reduce(balances, nil, &(min(&1, &2)))
    spending=being_spent(txs)
    bs=block[:data][:bond_size]
    cond do
      poorest_balance < bs -> 
        IO.puts("poorest signer cant afford")
        false
      bs*ns<spending*3 -> 
        IO.puts("not enough bonds to spend that much")
        false
      true -> true
    end
  end
  def blockhash(block) do
    if :data in Dict.keys(block) do block=block[:data] end
    DetHash.doit(block)
    #prev_block=KV.get("height")-1
    #DetHash.doit([prev_block[:hash], txs])
  end
  def buy_block do
    height=KV.get("height")
    block = get_block(KV.get("height"))
    txs=Mempool.txs#remove expensive txs until we can afford it. packing problem.
    bh=blockhash(block)
    new=[height: height+1, txs: txs, hash: bh, bond_size: 10_000_000_000_000/Constants.signers_per_block*3]
    new = Keys.sign(new)
    Dict.put(new, :meta, [revealed: []])
  end
  #def blocks_from_hash(hash, n \\ 50) do
  #Listener.flip(blocks_from_2(hash, n))
  #end
  #def blocks_from_2(hash, n) do
  #  cond do
  #    hash == nil or n == 0 -> []
  #    true ->
  #      block = BlockchainPure.get_block(hash)
  #      [block|blocks_from_hash(block[:data][:hash], n-1)]
  #  end
  #end
end
