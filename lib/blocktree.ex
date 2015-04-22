defmodule Blocktree do
  def blockhash(block) do
    if :data in Dict.keys(block) do block=block[:data] end
    DetHash.doit(block)
  end
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: []]]
    KV.put("height", 0)
    KV.put("0", ["genesis"])
    KV.put("genesis", new)
    genesis_block = [meta: [revealed: []], pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", sig: "MEYCIQCu3JqIcIn3jqBhH0nqF8ZTJmdV9GzlJ6WpSq66PA20sAIhAINAuEyCyl2x/iK3BRJM0JGXcd8epnzv0kTX6iHOMAeW", data: [height: 1, txs: [], hash: "z5cVID5hEmZcWNVRmVPRUtSN7e2Z5nXecc+8KWbxk+4=", bond_size: 3.0e11]]
    put_block(genesis_block)
    KV.put("height", 1)
    KV.put("1", [blockhash(genesis_block)])
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def put_block(block) do
    height = block[:data][:height] 
    block_hash = blockhash(block)
    block_hashes = height |> Blockchain.get_helper
    if block_hash in block_hashes do false else
      if block_hashes ==Constants.empty_account do block_hashes = [] end
      block_hashes = block_hashes++[block_hash]
      KV.put(to_string(height), block_hashes)
      KV.put(block_hash, Dict.put(block, :meta, [revealed: []]))
      block_hash
    end
  end
  def genesis_state do
    genesis_block
    a=Constants.empty_account
    ac=Constants.initial_coins
    b = ac/21
    a = Dict.put(a, :amount, 20*b)
    a = Dict.put(a, :bond, b)
    Keys.master
    creator_pub = Keys.pubkey
    KV.put(creator_pub, a)
    KV.put("tot_bonds", b)
    KV.put("", [height: 0, hash: ""])
    sign_reveal
  end
  def quick_validation(block) do
    #IO.puts("block #{inspect block}")
    ran=VerifyTx.rng(block[:data][:hash])
    tot_bonds = KV.get("tot_bonds")
    l = Blockchain.txs_filter(block[:data][:txs], "sign")
    |> Enum.map(fn(sign) -> 
      acc = KV.get(sign[:pub])
      Enum.map(sign[:data][:winners], fn(x)-> 
        VerifyTx.winner?(acc[:bond], tot_bonds, ran, sign[:pub], x) 
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
  def add_blocks(blocks) do#make sure the networking nodes can pass >30 blocks before calling this function.
    cond do
      blocks == [] -> []
      not :pub in Dict.keys(hd(blocks)) -> add_blocks(tl(blocks))
      not enough_validated(blocks, round(length(KV.get(hd(blocks)[:data][:height]))/3)) ->#should say "KV.get" in this line!!
         IO.puts("double-signing everywhere")
         false
      KV.get(blockhash(hd(blocks))) != Constants.empty_account ->
        IO.puts("already have this block")
        add_blocks(tl(blocks))
      true ->
        block_hash = put_block(hd(blocks))
        current_height = KV.get("height")
        if hd(blocks)[:data][:height]>current_height do Blockchain.goto(block_hash) end
        add_blocks(tl(blocks))
    end
  end
end
