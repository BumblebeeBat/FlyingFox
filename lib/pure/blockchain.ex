defmodule Blockchain do
  #The part of the blocktree we care about is the blockchain.
  #it ends in the most recent valid block.
  #@signers_per_block Application.get_env :flying_fox, :signers_per_block
  #@block_creation_fee Application.get_env :flying_fox, :block_creation_fee
  def txs_filter(txs, type) do Enum.filter(txs, fn(t) -> t.data.__struct__ == type end) end
  def being_spent(txs) do txs |> txs_filter(:Elixir.Spend) |> Enum.map(fn(t) -> t.data.amount end) |> Enum.reduce(0, &(&1+&2)) end
  def prev_block(block) do
		cond do
			block == nil -> nil
			true ->KV.get(block.data.hash)
		end
	end
  def valid_block?(block, cost) do
		#block creator needs to pay a fee. he needs to have signed so we can take his fee.
		f = fn(x) ->
			cond do
				x == nil -> 0
				x.data == nil -> 0
				x.data.bond_size == nil -> 0
				true ->	x.data.bond_size
			end
		end
		prev = prev_block(block)
    prev2 = prev_block(prev)
    prev3 = prev_block(prev2)
    min_bond = max(f.(prev), max(f.(prev2), f.(prev3)))
    if min_bond == nil do min_bond = 100000 end
    ngenesis = block.data.height != 1
    cond do
      not is_map(block) ->
        IO.puts("block should be a map #{inspect block}")
        false
      min_bond*2/3>f.(block) ->
        #if the amount of money bonded per block changes too quickly,
        #then it makes it more likely for double-spends to happen.
        IO.puts("not enough bonded")
        false
      ngenesis and prev == nil ->
        IO.puts("blocks come from parents: #{inspect block}")
        IO.puts(inspect block.data.height)
        false
      ngenesis and block.data.height - prev.data.height < 1 ->
        IO.puts("cannot redo history")
        false
      not CryptoSign.verify_tx(block) ->
        IO.puts("bad signature #{inspect block}")
        false
      true ->
        valid_block_2?(block, cost, ngenesis)
    end
  end
  def winners(block) do 
    block.data.txs
    |> txs_filter(:Elixir.Sign)
    |> Enum.map(&(length(&1.data.winners)))
    |> Enum.reduce(0, &(&1+&2))
  end
  def valid_block_2?(block, cost, ngenesis) do
    wins = winners(block)
    cond do
      ngenesis and wins < Constants.signers_per_block*2/3 ->
        IO.puts("not enough signers #{inspect wins}")
        IO.puts("block: #{inspect block}")
        false
      not VerifyTx.check_txs(block, cost) ->
        IO.puts("invalid tx")
        false
      true -> valid_block_3?(block, wins)
    end
  end
  def valid_block_3?(block, ns) do
    txs = block.data.txs
    sign_txs = txs_filter(txs, :Elixir.Sign)
    signers = Enum.map(sign_txs, fn(t) -> t.data.pub end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s.bond end)
    poorest_balance = Enum.reduce(balances, nil, &(min(&1, &2)))
    spending = being_spent(txs)
    bs = block.data.bond_size
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
  def get_helper(h) do KV.get(to_string(h)) end
  def get_block(h) do
		cond do
			is_integer(h) -> get_block_2(KV.get(to_string(h)))
			true -> get_block_3(h)#KV.get(h)
		end
	end
	def get_block_2(a) do
		cond do
			is_list(a) -> get_block_3(hd(a))#I guess the list should be another struct, and I should check it's struct type instead.
			true -> null_block
		end
	end
	def get_block_3(h) do
		h = KV.get(h)
		cond do
			h==nil -> null_block
			true -> h
		end
	end
	def null_block do %CryptoSign{data: %Block{}} end
  def blockhash(block) do
    case block do
      %CryptoSign{} -> block = block.data
      _ -> 1
    end
    %Block{} = block
    DetHash.doit(block)
  end
	def quick_validation(block) do
    ran = VerifyTx.rng(block.data.hash)
    tot_bonds = KV.get("tot_bonds")
    l = Blockchain.txs_filter(block.data.txs, :Elixir.Sign)
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
  def buy_block(n \\ 1) do
		true = n > 0
		height = KV.get("height")
		prev_block = get_block(KV.get("height"))
		txs = Mempool.txs#remove expensive txs until we can afford it. packing problem.
		bh = nil
		if prev_block != nil do
			bh = blockhash(prev_block)
		end
		new = %Block{height: height+n, txs: txs, hash: bh, bond_size: 1_000_000_000, pub: Keys.pubkey}#instead of fixed bond size, we shoul look at how big of a bond the txs need.
		|> Keys.sign
	end
end
