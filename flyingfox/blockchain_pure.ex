defmodule BlockchainPure do
  def get_helper(h) do KV.get(to_string(h)) end#ran 1444 times to add first 2 blocks?!?!
  def get_block(h) do#ran 1444 times to add first 2 blocks?!?!
    if is_integer(h) do h=hd(get_helper(h)) end
    KV.get(h)
  end
  def put_block(block) do
    height = block[:data][:height] 
    block_hash = blockhash(block)
    block_hashes = height |> get_helper
    if block_hashes ==Constants.empty_account do block_hashes = [] end
    block_hashes = block_hashes++[block_hash]
    KV.put(to_string(height), block_hashes)
    KV.put(block_hash, Dict.put(block, :meta, [revealed: []]))
    block_hash
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
  def prev_block(block) do KV.get(block[:data][:hash]) end
  def valid_block?(block) do 
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    prev = prev_block(block)
    ngenesis = block[:data][:height]!=1
    cond do
      not is_list(block) -> 
        IO.puts("block should be a dict #{inspect block}")
        false
      ngenesis and prev == Constants.empty_account ->
        IO.puts("blocks come from parents: #{inspect block}")
        IO.puts(inspect block[:data][:height])
        false
      ngenesis and prev[:data][:height]+1 != block[:data][:height] ->
        IO.puts("incorrect height")
        false
      not Sign.verify_tx(block) -> 
        IO.puts("bad signature #{inspect block}")
        false
      true ->
        valid_block_2?(block)
    end
  end
  def winners(block) do block[:data][:txs] |> txs_filter("sign") |> Enum.map(&(length(&1[:data][:winners]))) |> Enum.reduce(0, &(&1+&2)) end
  def valid_block_2?(block) do
    wins = winners(block)
    cond do
      wins < Constants.signers_per_block*2/3 -> 
        IO.puts("not enough signers #{inspect wins}")
        IO.puts("block: #{inspect block}")
        false
      not VerifyTx.check_txs(block) ->
        IO.puts("invalid tx")
        false
      true -> valid_block_3?(block, wins) 
    end
  end
  def valid_block_3?(block, ns) do
    txs = block[:data][:txs] 
    sign_txs=txs_filter(txs, "sign")
    signers = Enum.map(sign_txs, fn(t) -> t[:pub] end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
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
  end
  def buy_block do
    height=KV.get("height")
    prev_block = get_block(KV.get("height"))
    txs=Mempool.txs#remove expensive txs until we can afford it. packing problem.
    if prev_block==Constants.empty_account do bh=nil else
      bh=blockhash(prev_block)
    end
    new=[height: height+1, txs: txs, hash: bh, bond_size: 10_000_000_000_000/Constants.signers_per_block*3]
    new = Keys.sign(new)
    Dict.put(new, :meta, [revealed: []])
  end
end
