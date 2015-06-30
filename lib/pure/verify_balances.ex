defmodule VerifyBalances do
  def all_positive(a) do
    cond do
      a == [] -> true
      true -> all_positive_1(a)
    end
  end
  def all_positive_1(addresses) do
    [{_, t}|tail] = addresses
    cond do
      t < 0 -> false
      tail == [] -> true
      true -> all_positive_1(tail)
    end
  end
  def get([head|tail], key) do
    {k, v} = head
    cond do
      k==key -> v
      true -> get(tail, key)
    end
  end
  def put(l, key, value) do
    cond do
      l==[] -> [{key, value}]
      key == elem(hd(l), 0) -> [{key, value}|tl(l)]
      true -> [hd(l)|put(tl(l), key, value)]
    end
  end
  def modify_balance(addresses, pub, f, key) do
    balance = get(addresses, pub)
    balance = f.(balance)
    [{key, balance}|addresses]
    put(addresses, key, balance)
  end
  def lose_key(address, pub, amount, key) do
    f = fn(balance) ->
      Dict.put(balance, key, Dict.get(balance, key) - amount) 
    end
    modify_balance(address, pub, f, key)
  end
  def lose_cash(address, pub, amount) do
    lose_key(address, pub, amount, :cash)
  end
  def lose_bond(address, pub, amount) do
    lose_key(address, pub, amount, :bond)
  end
  def positive_balances(txs, bond_size, block_creator, cost) do
		f = fn(acc) -> positive_balances_1_5(txs, acc, block_creator, bond_size, cost) end
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
	def positive_balances_1_5(txs, acc, block_creator, bond_size, cost) do
    balance = [cash: acc.amount, bond: acc.bond]
    addresses = [{block_creator, balance}]
    positive_balances_2(txs, bond_size, block_creator, cost, addresses)
  end
  def positive_balances_2(txs, bond_size, block_creator, cost, addresses) do
    cond do
      txs==[] ->
				if block_creator != nil do
          addresses = lose_cash(addresses, block_creator, cost) 
        end
				#IO.puts("verify balances #{inspect addresses}")#verify balances [{"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", [cash: 1999999689989990.0, bond: 99734203939204.69]}, {"BDju4ADMQhB0DtzrM8BeVHZlVD74QVKXCfhpcvH8yg5/7yaOK7/e6mig4RC8WVpaVowInI4lMMHlV/UJKbEtBck=", [cash: 10, bond: 0]}, {:bond, [bond: 99734203939204.69, cash: 1999999689989990.0]}, {:cash, [cash: -61999990, bond: 0]}]
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
      addresses = [{pub, balance}|addresses]
    end
    case da.__struct__ do
      :Elixir.Spend        -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Spend2Wait   -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Wait2Bond    -> addresses = lose_cash(addresses, pub, da.fee)
      :Elixir.Bond2Spend   -> addresses = lose_bond(addresses, pub, da.amount) |> lose_cash(pub, da.fee)
      :Elixir.Sign         -> addresses = lose_bond(addresses, pub, bond_size*length(da.winners))
      :Elixir.Slasher      -> true
      :Elixir.Reveal       -> true
      :Elixir.ToChannel    -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
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
