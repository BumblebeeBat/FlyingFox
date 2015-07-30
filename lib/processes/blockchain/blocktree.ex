defmodule Blocktree do
  def genesis_block do
    b = %Block{height: 1, txs: [], hash: "z5cVID5hEmZcWNVRmVPRUtSN7e2Z5nXecc+8KWbxk+4=", bond_size: 1_000_000, pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc="}
		m = %Meta{revealed: [], sig: "MEYCIQCu3JqIcIn3jqBhH0nqF8ZTJmdV9GzlJ6WpSq66PA20sAIhAINAuEyCyl2x/iK3BRJM0JGXcd8epnzv0kTX6iHOMAeW"}
		genesis_block = %CryptoSign{meta: m, data: b}
    put_block(genesis_block)
    KV.put("height", 1)
    KV.put("1", [Blockchain.blockhash(b)])
  end
  def sign_reveal do
    TxCreator.sign
    TxCreator.reveal
  end
  def put_block(signed) do
    block = signed.data
    height = block.height
    block_hash = Blockchain.blockhash(block)
    block_hashes = height |> Blockchain.get_helper
    if block_hashes == nil do block_hashes = [] end
    if block_hash in block_hashes do
			false
		else
      block_hashes = block_hashes++[block_hash]
      KV.put(to_string(height), block_hashes)
			signed = %{signed | meta: %{signed.meta | revealed: []}}
      KV.put(block_hash, signed)
			block_hash
    end
  end
  def genesis_state do
    genesis_block
    ac = Constants.initial_coins
    b = ac/3
    a = %Account{amount: 2*b, bond: b}
    #Keys.master
    creator_pub = Constants.creator_pub
    KV.put(creator_pub, a)
    KV.put("tot_bonds", b)
    #sign_reveal
  end
	def num_signers(txs) do txs |> Enum.filter(&(&1.data.__struct__ == :Elixir.Sign)) |> length end
	def back do
    h = KV.get("height")
    if h>0 do
      block = Blockchain.get_block(h)
      prev = Blockchain.get_block(block.data.hash)
      txs = block.data.txs
      n = num_signers(txs)
      TxUpdate.txs_updates(txs, -1, round(block.data.bond_size/n))
      TxUpdate.sym_increment(block.data.pub, :amount, -Constants.block_creation_fee, -1)
      b = prev.data.height
      if b == nil do b = 0 end
      KV.put("height", b)
      Mempool.dump
      true
    end
  end
  def forward(block) do#while walking forward this needs to reorder the hashes used for get_block so that the block we are using is on top.
    if not is_map(block) do block = KV.get(block) end
    #IO.puts("forward block #{inspect block}")
    gap = block.data.height-KV.get("height")
    cost = Constants.block_creation_fee * round(:math.pow(2, gap))
    cond do
      not is_map(block) -> [error: "blocks should be maps"]
      KV.get(Blockchain.blockhash(block)) == nil -> [error: "don't have this block"]
      #gap < 1 -> [error: "cannot redo history"]
      gap != 1 ->
				[error: "history moves one direction"]
      not Blockchain.valid_block?(block, cost) ->
        IO.puts("invalid block #{inspect block}")
        false
      true ->
        #block creator needs to pay a fee. he needs to have signed so we can take his fee.
        TxUpdate.sym_increment(block.data.pub, :amount, -cost, 1)
        txs = block.data.txs
        n = num_signers(txs)
        TxUpdate.txs_updates(txs, 1, round(block.data.bond_size/n))
        KV.put("height", block.data.height)
        Mempool.dump
        hash = Blockchain.blockhash(block)
        n = to_string(block.data.height)
        bh = KV.get(n) |> Enum.filter(&(&1!=hash))
        KV.put(n, [hash|bh])
    end
  end
	def goto(hash, acc \\ []) do
		goal_chain_block = hash |> Blockchain.get_block
		my_chain_block = goal_chain_block.data.height |> Blockchain.get_block
		if my_chain_block == goal_chain_block do
			goto_helper([goal_chain_block|acc])
		else
			goto(goal_chain_block.data.hash, [goal_chain_block|acc])
		end
	end
	def goto_helper(last_blocks) do
		#this should back up until we are on the same fork, then walk forward through last_blocks till the end.
    h = KV.get("height")
    my_block = Blockchain.get_block(h).data
		add_block = hd(last_blocks).data
		hash = Blockchain.get_block(add_block.height).data.hash
    cond do
			add_block.hash == hash ->
				forward(hd(last_blocks))
				if tl(last_blocks) != [] do	goto_helper(tl(last_blocks)) end
      add_block.height > my_block.height+1 ->
        goto_helper([Blockchain.get_block(add_block.hash)|last_blocks])
      true ->
        IO.puts("back")
        back
        goto_helper(last_blocks)
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
		acc = KV.get(block.pub)
    cond do
			block.bond_size > Constants.max_bond ->
				add_blocks(tail)
				false
      not Blockchain.enough_validated([head], round(length(get_height(height))/3)) ->
        IO.puts("double-signing everywhere")
				add_blocks(tail)
      KV.get(Blockchain.blockhash(head)) != nil -> add_blocks(tail)
      true ->
				IO.puts("adding block #{inspect height}")
        block_hash = put_block(head)
        current_height = KV.get("height")
        if height > current_height do goto(block_hash) end
        add_blocks(tail)
    end
  end
end
