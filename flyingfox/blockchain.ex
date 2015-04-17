defmodule Blockchain do
  def genesis_block do
    new=[meta: [revealed: []], data: [height: 0, txs: []]]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def buy_block(b) do
    a = add_block(b)
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
    IO.puts("back!!!")
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
  def forward(block) do
    if not is_list(block) do block = KV.get(block) end
    h = KV.get("height")
    cond do
      block == Constants.empty_account ->
        [error: "sub-zero block"]
      block == nil ->
        [error: "nil block"]
      h + 1 != block[:data][:height] ->
        [error: "bad height"]
      true ->
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        txs=block[:data][:txs]
        KV.put(to_string(h+1), BlockchainPure.blockhash(block))
        n=BlockchainPure.num_signers(txs)
        TxUpdate.txs_updates(txs, 1, round(block[:data][:bond_size]/n))
        KV.put("height", h+1)
        Mempool.dump
    end
  end
  def add_block(block) do
    cond do
      not BlockchainPure.valid_block?(block) -> 
        IO.puts("invalid block")
        false
      #if there are already 5 or more blocks at this height, then do not add this block. maybe use KV("5") to store a list of up to 5 hashes.
      true ->
        IO.puts("valid block")
        hash = BlockchainPure.blockhash(block)
        KV.put(hash, Dict.put(block, :meta, [revealed: []]))
        #biggest = BlockchainPure.get_block(KV.get("highest"))
        h = block[:data][:height]
        current_height = KV.get("height")
        if h > current_height do
          goto(hash)
        end
    end
  end
  def goto(hash) do
    h = hash |> BlockchainPure.get_block
    goto_helper([h])
  end
  def goto_helper(last_blocks, my_block \\ []) do
    h = KV.get("height")
    if h==0 do 
      my_block = :genesis 
      hash = ""
    else
      if my_block==[] do my_block=BlockchainPure.get_block(h)[:data] end
      hash = BlockchainPure.blockhash(my_block)
    end
    add_block = hd(last_blocks)[:data]
    cond do
      my_block == :genesis or add_block[:hash] == hash -> 
        IO.puts("add blocks: #{inspect last_blocks}")
        Enum.map(last_blocks, &(forward(&1)))
      add_block[:height] > my_block[:height] + 1 ->
        goto_helper(last_blocks)
      true ->
        IO.puts("back")
        back
        goto_helper(last_blocks)
    end
  end
end
