defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: []]]
    KV.put("height", 0)
    KV.put("0", ["genesis"])
    KV.put("genesis", new)
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def buy_block(b) do
    a = add_blocks([b])
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
    Keys.master
    creator_pub = Keys.pubkey
    KV.put(creator_pub, a)
    KV.put("tot_bonds", b)
    KV.put("", [height: 0, hash: ""])
    sign_reveal
  end
  def back do
    h=KV.get("height")
    if h>0 do
        block=BlockchainPure.get_block(h)
        prev = BlockchainPure.get_block(block[:data][:hash])
        txs=block[:data][:txs]
        n=BlockchainPure.num_signers(BlockchainPure.txs_filter(txs, "sign"))
        TxUpdate.txs_updates(txs, -1, round(block[:data][:bond_size] / n))
        TxUpdate.sym_increment(block[:pub], :amount, -Constants.block_creation_fee, -1)
        b = prev[:data][:height]
        if b==nil do b=0 end
        KV.put("height", b)
        Mempool.dump
        true 
    end
  end
  def forward(block) do#while walking forward this needs to reorder the hashes used for get_block so that the block we are using is on top. I thought we only store one of the blockhashes...
    if not is_list(block) do block = KV.get(block) end
    gap = block[:data][:height]-KV.get("height")
    cost = Constants.block_creation_fee*round(:math.pow(2, gap))
    cond do
      not is_list(block) -> [error: "blocks should be lists"]
      KV.get(BlockchainPure.blockhash(block)) == Constants.empty_account -> [error: "don't have this block"]
      gap < 1 -> [error: "cannot redo history"]
      not BlockchainPure.valid_block?(block, cost) -> 
        IO.puts("invalid block")
        false      
      true ->
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        TxUpdate.sym_increment(block[:pub], :amount, -cost, 1)#if I skip blocks, charge more
        txs=block[:data][:txs]
        #BlockchainPure.put_block(block)
        n=BlockchainPure.num_signers(txs)
        TxUpdate.txs_updates(txs, 1, round(block[:data][:bond_size]/n))
        KV.put("height", block[:data][:height])
        Mempool.dump
        hash = BlockchainPure.blockhash(block)
        n = to_string(block[:data][:height])
        bh = KV.get(n) |> Enum.filter(&(&1!=hash))
        KV.put(n, [hash|bh])
    end
  end
  def quick_validation(block) do
    #IO.puts("block #{inspect block}")
    ran=VerifyTx.rng(block[:data][:hash])
    tot_bonds = KV.get("tot_bonds")
    l = BlockchainPure.txs_filter(block[:data][:txs], "sign")
    |> Enum.map(fn(sign) -> 
      acc = KV.get(sign[:pub])
      Enum.map(sign[:data][:winners], fn(x)-> 
        VerifyTx.winner?(acc[:bond], tot_bonds, ran, sign[:pub], x) 
      end) 
    end) |> Enum.reduce([], &(&1++&2)) |> Enum.map(&(if(&1) do 1 else 0 end)) |> Enum.reduce(0, &(&1+&2))
    cond do
      BlockchainPure.winners(block) <= Constants.signers_per_block*2/3 -> 
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
      KV.get(BlockchainPure.blockhash(hd(blocks))) != Constants.empty_account ->
        IO.puts("already have this block")
        add_blocks(tl(blocks))
      true ->
        block_hash = BlockchainPure.put_block(hd(blocks))
        current_height = KV.get("height")
        if hd(blocks)[:data][:height]>current_height do goto(block_hash) end
        add_blocks(tl(blocks))
    end
  end
  def goto(hash) do
    h = hash |> BlockchainPure.get_block
    goto_helper([h])
  end
  def goto_helper(last_blocks, my_block \\ []) do
    h = KV.get("height")
    if h==0 do 
      my_block = [height: 0]
      hash = ""
    else
      if my_block==[] do 
        my_block=BlockchainPure.get_block(h)[:data] 
      end
      hash = BlockchainPure.blockhash(my_block)#my_block is nil??
    end
    add_block = hd(last_blocks)[:data]
    cond do
      length(last_blocks)>60 -> 
        IO.puts("error!#! #{inspect last_blocks}")
      hd(last_blocks) == [amount: 0, bond: 0, wait: {0, 0}, nonce: 0] ->
        IO.puts("error 2 #{inspect last_blocks}")
        Enum.map(tl(last_blocks), &(forward(&1)))
      my_block[:height] == 0 or add_block[:hash] == hash -> 
        Enum.map(last_blocks, &(forward(&1)))
      add_block[:height] > my_block[:height] ->
        IO.puts("always here")
        goto_helper([BlockchainPure.get_block(add_block[:hash])|last_blocks])
      true ->
        IO.puts("back")
        back
        goto_helper(last_blocks)
    end
  end
end
