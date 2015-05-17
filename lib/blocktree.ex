defmodule Blocktree do

  def blockhash(block) do
    case block do
      %Signed{} -> block = block.data
      _ -> 1
    end
    %Block{} = block
    DetHash.doit(block)
  end

  def genesis_block do
    b = %Block{height: 1, txs: [], hash: "z5cVID5hEmZcWNVRmVPRUtSN7e2Z5nXecc+8KWbxk+4=", bond_size: 3.0e11}
    genesis_block = %Signed{meta: [revealed: []], pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", sig: "MEYCIQCu3JqIcIn3jqBhH0nqF8ZTJmdV9GzlJ6WpSq66PA20sAIhAINAuEyCyl2x/iK3BRJM0JGXcd8epnzv0kTX6iHOMAeW", data: b}
    put_block(genesis_block)
    KV.put("height", 1)
    KV.put("1", [blockhash(b)])
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def put_block(signed) do
    block = signed.data
    height = block.height
    block_hash = blockhash(block)
    block_hashes = height |> Blockchain.get_helper
    if block_hashes == nil do block_hashes = [] end
    if block_hash in block_hashes do false else
      block_hashes = block_hashes++[block_hash]
      KV.put(to_string(height), block_hashes)
      KV.put(block_hash, %{signed | meta: [revealed: []]})
      block_hash
    end
  end
  def genesis_state do
    genesis_block
    ac = Constants.initial_coins
    b = ac/21
    a = %Account{amount: 20*b, bond: b}
    Keys.master
    creator_pub = Keys.pubkey
    KV.put(creator_pub, a)
    KV.put("tot_bonds", b)
    sign_reveal
  end
  def quick_validation(block) do
    ran = VerifyTx.rng(block.data.hash)
    tot_bonds = KV.get("tot_bonds")
    l = Blockchain.txs_filter(block.data.txs, :Elixir.SignTx)
    |> Enum.map(fn(sign) ->
      %{bond: bond} = KV.get(sign.pub)
      Enum.map(sign.data.winners, fn(x)-> 
        VerifyTx.winner?(bond, tot_bonds, ran, sign.pub, x) 
      end)
    end) |> Enum.reduce([], &(&1++&2)) |> Enum.map(&(if(&1) do 1 else 0 end)) |> Enum.reduce(0, &(&1+&2))
    cond do
      Blockchain.winners(block) <= Constants.signers_per_block*2/3 -> 
        IO.puts("not enough winners")
        false
      l <= Constants.signers_per_block*1/2 -> 
        IO.puts("not enough")
        false
      true -> true
    end
  end
  def enough_validated(blocks, n) do
    cond do
      n == 0 -> true
      blocks == [] -> false
      not quick_validation(hd(blocks)) ->
        IO.puts("bad block")
        false
      true ->
        enough_validated(tl(blocks), n-1)
    end
  end
  def get_height(h) do
    a = KV.get(h)
    if a == nil do a = [] end
    a
  end
  def add_blocks([]) do [] end
  def add_blocks([head|tail]) do
    block = head.data
    height = block.height
    cond do
      not enough_validated([head|tail], round(length(get_height(height))/3)) ->
        IO.puts("double-signing everywhere")
        false
      KV.get(blockhash(head)) != nil ->
        IO.puts("already have this block: #{inspect head}")
        IO.puts("already: #{inspect KV.get(blockhash(head))}")
        add_blocks(tail)
      true ->
        block_hash = put_block(head)
        current_height = KV.get("height")
        if height > current_height do Blockchain.goto(block_hash) end
        add_blocks(tail)
    end
  end
end
