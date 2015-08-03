defmodule VerifyBalances do
	def all_positive(a) do
		a |> Enum.map(fn({_, x}) -> x[:cash] >= 0 and x[:bond] >= 0 end)
		|> Enum.reduce(true, &(&1 and &2))
	end
	def lose_cash(addresses, pub, amount) do
		acc = HashDict.get(addresses, pub)
		HashDict.put(addresses, pub, [cash: acc[:cash]-amount, bond: acc[:bond]])
	end
	def lose_bond(addresses, pub, amount) do
		acc = HashDict.get(addresses, pub)
		HashDict.put(addresses, pub, [cash: acc[:cash], bond: acc[:bond]-amount])
	end
  def positive_balances(txs, bond_size, block_creator, cost) do
		f = fn(acc) ->
			addresses = HashDict.put(%HashDict{}, block_creator, [cash: acc.amount, bond: acc.bond])
			positive_balances_2(txs, bond_size, block_creator, cost, addresses)
		end
		g = KV.get(block_creator)
		cond do
			block_creator == ""  ->
				#The point of this is to do calculations before we have decided who the block_creator will be.
				f.(%Account{amount: Constants.initial_coins})
			g == nil ->
				IO.puts("non-existent account cannot make block #{inspect block_creator}")
				false
			true -> f.(g)
		end
	end
  def positive_balances_2(txs, bond_size, block_creator, cost, addresses) do
    cond do
      txs==[] ->
				if block_creator != "" do
          addresses = lose_cash(addresses, block_creator, cost) 
        end
        all_positive(addresses)
      true -> positive_balances_1(txs, bond_size, block_creator, cost, addresses)
    end
  end
  def positive_balances_1(txs, bond_size, block_creator, cost, addresses) do
    [tx|txs] = txs
    pub = tx.data.pub
    da = tx.data
    if not pub in Dict.keys(addresses) do
      acc = KV.get(pub)
      balance = [cash: acc.amount, bond: acc.bond]
			addresses = HashDict.put(addresses, pub, balance)
    end
    case da.__struct__ do
      :Elixir.Spend        -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Spend2Wait   -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Wait2Bond    -> addresses = lose_cash(addresses, pub, da.fee)
      :Elixir.Bond2Spend   -> addresses = lose_bond(addresses, pub, da.amount) |> lose_cash(pub, da.fee)
      :Elixir.Sign         -> addresses = lose_bond(addresses, pub, bond_size*length(da.winners))
      :Elixir.Slasher      -> true
      :Elixir.Reveal       -> true
      :Elixir.ToChannel    ->
				IO.puts("verify balances #{inspect addresses}")
				IO.puts("verify balances #{inspect pub}")
				IO.puts("verify balances #{inspect da.amount+da.fee}")
				addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.ChannelBlock -> true
      :Elixir.CloseChannel -> true
      :Elixir.Oracle       -> addresses = lose_cash(addresses, pub, Constants.oracle_fee) #maybe it is easiest if one person pays for the oracle.
      :Elixir.Judgement    -> true
      x -> 
        IO.puts("no function with that name #{inspect x}")
        false
    end
    positive_balances_2(txs, bond_size, block_creator, cost, addresses)
  end
end
