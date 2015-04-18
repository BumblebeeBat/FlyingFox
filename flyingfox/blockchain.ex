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
        txs=block[:data][:txs]
        n=BlockchainPure.num_signers(BlockchainPure.txs_filter(txs, "sign"))
        TxUpdate.txs_updates(txs, -1, round(block[:data][:bond_size] / n))
        #give block creator his fee back.
        KV.put("height", h-1)
        Mempool.dump
        true 
    end
  end
  def forward(block) do#while walking forward this needs to reorder the hashes used for get_block so that the block we are using is on top. I thought we only store one of the blockhashes...
    if not is_list(block) do block = KV.get(block) end
    h = KV.get("height")
    cond do
      not is_list(block) -> [error: "blocks should be lists"]
      KV.get(BlockchainPure.blockhash(block)) == Constants.empty_account -> [error: "don't have this block"]
      h + 1 != block[:data][:height] -> 
        [error: "bad height"]
      not BlockchainPure.valid_block?(block) -> 
        IO.puts("invalid block")
        false      
      true ->
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        txs=block[:data][:txs]
        BlockchainPure.put_block(block)
        n=BlockchainPure.num_signers(txs)
        TxUpdate.txs_updates(txs, 1, round(block[:data][:bond_size]/n))
        KV.put("height", h+1)
        Mempool.dump
    end
  end
  def enough_validated(blocks, n) do
    cond do
      n == 0 -> true
      blocks == [] -> false
      hd(blocks) == [meta: [revealed: []], data: [height: 0, txs: []]] -> enough_validated(tl(blocks), n) 
      BlockchainPure.winners(hd(blocks)) > Constants.signers_per_block*19/30 -> enough_validated(tl(blocks), n-1)
      true ->
        IO.puts("enough validated #{inspect n} #{inspect blocks}")
        IO.puts("how many #{inspect BlockchainPure.winners(hd(blocks))}")
        false
    end
  end
  def add_blocks(blocks) do#this is doing fork_case every time. that is wrong. only do that sometimes...
    add_blocks_helper(blocks)
    
  end
  def add_blocks_helper(blocks) do#make sure the networking nodes can pass >30 blocks before calling this function.
    cond do
      blocks == [] -> []
      not enough_validated(blocks, round(length(BlockchainPure.get_block(hd(blocks)[:data][:height]))/3)) ->
         IO.puts("double-signing everywhere")
         false
      KV.get(BlockchainPure.blockhash(hd(blocks))) != Constants.empty_account ->
        IO.puts("already have this block")
        add_blocks_helper(tl(blocks))
      true ->
        IO.puts("valid block #{inspect hd(blocks)[:data][:height]}")
        block_hash = BlockchainPure.put_block(hd(blocks))
        current_height = KV.get("height")
        if hd(blocks)[:data][:height]>current_height do goto(block_hash) end
        add_blocks_helper(tl(blocks))
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
      if my_block==[] do my_block=BlockchainPure.get_block(h)[:data] end
      hash = BlockchainPure.blockhash(my_block)
    end
    add_block = hd(last_blocks)[:data]
    cond do
      add_block[:height] > my_block[:height] + 1 ->
        goto_helper([BlockchainPure.get_block(add_block[:hash])|last_blocks])#hit this row every time. infinite loop.
      my_block[:height] == 0 or add_block[:hash] == hash -> 
        Enum.map(last_blocks, &(forward(&1)))
      true ->
        IO.puts("back")
        back
        goto_helper(last_blocks)
    end
  end
end
