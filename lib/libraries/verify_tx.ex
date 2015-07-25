defmodule VerifyTx do
  def check_tx(tx, txs, prev_hash) do
    cond do
      not check_logic(tx, txs, prev_hash) -> false
      not check_(%{pub: nil, data: %Block{txs: [tx|txs]}}, Constants.block_creation_fee) -> false
      true -> true
    end
  end
  def remove_repeats(l) do
    cond do
      l == [] -> l
      hd(l) in tl(l) -> remove_repeats(tl(l))
      true -> [hd(l)|remove_repeats(tl(l))]
    end
  end
  def consecutive?(l) do
    # l is a list of numbers
    cond do
      length(l) < 2 -> true
      hd(l) + 1 == hd(tl(l)) -> consecutive?(tl(l))
      true -> false
    end
  end
  def check_nonces(txs) do
		#remove any channel blocks and close_channels with type fast.
		txs = Enum.filter(txs, fn(tx) -> tx.data.__struct__ != :Elixir.ChannelBlock end)
    have_nonce = Enum.map(txs, fn(tx) -> tx.data.nonce != nil end)
    all_have_nonce = Enum.reduce(have_nonce, true, &(&1 and &2))
    pubs = txs
    |> Enum.map(fn(tx) -> tx.data.pub end)
    |> remove_repeats
    sorted_txs = Enum.map(pubs, fn(pub) -> Enum.filter(txs,  &(&1.data.pub == pub)) end)
    f = (fn(x) -> Enum.reduce(x, true, &(&1 and &2)) end)
    just_nonces = Enum.map(sorted_txs, fn(ts) -> Enum.map(ts, &(&1.data.nonce)) |> Enum.sort end)
    consecutive = just_nonces
    |> Enum.map(&(consecutive?(&1))) 
    |> f.()
		#IO.puts("pubs #{inspect txs}")
    current_nonce = Enum.map(pubs, fn(pub) -> KV.get(pub).nonce end)
    starts_right = Enum.zip(current_nonce, just_nonces)
    |> Enum.map(fn(x)-> elem(x, 0)==hd(elem(x, 1)) end) 
    |> f.()
    (starts_right and consecutive) and all_have_nonce
   end
   def check_(block, cost) do
     txs = block.data.txs
     spending = Blockchain.being_spent(txs)
     winners = txs
     |> Blockchain.txs_filter(:Elixir.Sign)
     |> Enum.map(fn(t) -> t.data.winners end)
     |> Enum.map(fn(w) -> length(w) end)
     |> Enum.reduce(0, &(&1+&2))
		 #IO.puts("check_ #{inspect block}")
     cond do
       not check_nonces(txs) -> false
       not VerifyBalances.positive_balances(txs, spending*3/max(winners, Constants.signers_per_block*2/3), block.data.pub, cost)->
         IO.puts("someone spent more money than how much they have")
         IO.puts("txs #{inspect txs}")
         IO.puts("block #{inspect block}")
         false
       true -> true
     end
  end
  def check_txs(block, cost) do#accept block as input
    da = block.data
    txs = da.txs
    prev_hash = da.hash
    cond do
      not check_logics(txs, prev_hash, []) ->
        IO.puts("bad logic")
        false
      txs == [] ->
        IO.puts("no empty blocks")
        false
      not check_(block, cost) -> false
      true -> true
    end
  end
	def check_logic(tx, txs, prevhash) do
		k = tx.data.__struct__
		f = case k do
					:Elixir.Sign -> &(Sign.check(&1, &2, prevhash))
					:Elixir.Spend -> &(Spend.check(&1, &2))
					:Elixir.Spend2Wait -> &(Spend2Wait.check(&1, &2))
					:Elixir.Wait2Bond -> &(Wait2Bond.check(&1, &2))
					:Elixir.Bond2Spend -> &(Bond2Spend.check(&1, &2))
					:Elixir.Slasher -> &(Spend.check(&1, &2))
					:Elixir.Reveal -> &(Reveal.check(&1, &2))
					:Elixir.ToChannel -> &(ToChannel.check(&1, &2))
					:Elixir.ChannelBlock -> &(ChannelBlock.check(&1, &2))
					:Elixir.CloseChannel -> &(CloseChannel.check(&1, &2))
					:Elixir.Oracle -> &(Oracle.check(&1, &2))
					:Elixir.Judgement -> &(Judgement.check(&1, &2))
					:Elixir.Win -> &(Win.check(&1, &2))
					_ ->
						IO.puts("invalid tx type: #{inspect k}")
					  fn(_, _) -> false end
			end
		cond do
			not f.(tx, txs) ->
				#IO.puts("bad tx")
				false
			not CryptoSign.verify_tx(tx) ->
				IO.puts("bad signature checklogic #{inspect tx}")
				true = false
				false
			true -> true
		end
	end
  def check_logics(new, prev_hash, old \\ []) do
    cond do
      length(new) == 0 -> true
      check_logic(hd(new), old, prev_hash) -> check_logics(tl(new), prev_hash, [hd(new)|old])
      true ->
				IO.puts("check_logics returns false #{inspect hd(new)}")
				false
    end
  end
end
