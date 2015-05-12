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
    acc = KV.get(block_creator)
    balance = [cash: acc[:amount], bond: acc[:bond]]
    addresses = [{block_creator, balance}]
    positive_balances_2(txs, bond_size, block_creator, cost, addresses)
  end
  def positive_balances_2(txs, bond_size, block_creator, cost, addresses) do
    cond do
      txs==[] -> if block_creator != nil do
                   addresses = lose_cash(addresses, block_creator, cost) 
                 end
                 all_positive(addresses)
      true -> positive_balances_1(txs, bond_size, block_creator, cost, addresses)
    end
  end
  def positive_balances_1(txs, bond_size, block_creator, cost, addresses) do
    [tx|txs] = txs
    pub = tx.pub
    da = tx.data
    if not pub in Dict.keys(addresses) do
      acc = KV.get(pub)
      balance = [cash: acc.amount, bond: acc.bond]
      addresses = [{pub, balance}|addresses]
    end
    case da.__struct__ do
      :Elixir.SpendTx        -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Spend2WaitTx   -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.Wait2BondTx    -> addresses = lose_cash(addresses, pub, da.fee)
      :Elixir.Bond2SpendTx   -> addresses = lose_bond(addresses, pub, da.amount) |> lose_cash(pub, da.fee)
      :Elixir.SignTx         -> addresses = lose_bond(addresses, pub, bond_size*length(da.winners))
      :Elixir.SlasherTx      -> true
      :Elixir.RevealTx       -> true
      :Elixir.ToChannelTx    -> addresses = lose_cash(addresses, pub, da.amount+da.fee)
      :Elixir.ChannelBlockTx -> true
      :Elixir.CloseChannelTx -> true
      x -> 
        IO.puts("no function with that name #{inspect x}")
        false
    end
    positive_balances_2(txs, bond_size, block_creator, cost, addresses)
  end
end
