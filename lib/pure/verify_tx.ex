defmodule VerifyTx do
	def check_sig2(tx) do tx |> Dict.put(:sig, tx.sig2) |> Dict.put(:pub, tx.pub2) |> CryptoSign.verify_tx() end
  def check_tx(tx, txs, prev_hash) do
    cond do
      not CheckLogic.main(tx, txs, prev_hash) -> false
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
    have_nonce = Enum.map(txs, fn(tx) -> tx.data.nonce != nil end)
    all_have_nonce = Enum.reduce(have_nonce, true, &(&1 and &2))
    pubs = txs
    |> Enum.map(fn(tx) -> tx.pub end)
    |> remove_repeats
    sorted_txs = Enum.map(pubs, fn(pub) -> Enum.filter(txs,  &(&1.pub == pub)) end)
    f = (fn(x) -> Enum.reduce(x, true, &(&1 and &2)) end)
    just_nonces = Enum.map(sorted_txs, fn(ts) -> Enum.map(ts, &(&1.data.nonce)) 
                                                 |> Enum.sort
    end)
    consecutive = just_nonces
    |> Enum.map(&(consecutive?(&1))) 
    |> f.()
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
       not VerifyBalances.positive_balances(txs,spending*3/max(winners, Constants.signers_per_block*2/3), block.pub, cost)->
         IO.puts("someone spent more money than how much they have")
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
  def check_logics(new, prev_hash, old \\ []) do
    cond do
      length(new) == 0 -> true
      CheckLogic.main(hd(new), old, prev_hash) -> check_logics(tl(new), prev_hash, [hd(new)|old])
      true ->
				IO.puts("check_logics returns false #{inspect hd(new)}")
				false
    end
  end
end
