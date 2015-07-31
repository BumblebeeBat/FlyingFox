defmodule ToChannel do
	#pub is always the person who is spending money into the channel.
  defstruct nonce: 0, to: "amount", amount: 0, new: false, delay: 100, fee: 10000, pub: "", pub2: "", create: false
	def key(a, b) do
		IO.puts("to channel key #{inspect a} #{inspect b}")
		cond do
			a > b -> a <> b
			true -> b <> a
		end
	end
	def check(tx, txs) do
		news = txs |> Enum.filter(&(&1.data.__struct__ == tx.data.__struct__))
		|> Enum.filter(&(key(&1.data.pub, &1.data.pub2) == key(tx.data.pub, tx.data.pub2)))
		|> Enum.filter(&(&1.data.new == true))
		acc = KV.get(tx.data.pub2)
    cond do
			tx.data.pub == tx.data.pub2 ->
				IO.puts("no channel with yourself")
				false
			tx.data.amount <= 0 ->
				IO.puts("cant send negative amounts")
				false
			KV.get(tx.data.pub) == nil ->
				IO.puts("account hasn't been registered #{inspect tx.data.pub}")
				false
			acc == nil and not tx.data.create ->
				IO.puts("this account doesn't exist yes")
				false
			acc != nil and tx.data.create ->
				IO.puts("this account already exists")
				false
			tx.data.new and news != [] ->
				IO.puts("no repeated news")
				false
      not tx.data.to in ["amount", "amount2"] ->
				IO.puts("bad to #{inspect tx}")
				false
			tx.data.pub == nil ->
				IO.puts("nil pub #{inspect tx.data.pub}")
				false
			tx.data.pub2 == nil ->
				IO.puts("nil pub 2 #{inspect tx.data.pub2}")
				IO.puts("tx #{inspect tx}")
				false
			true -> check_2(tx, KV.get(key(tx.data.pub, tx.data.pub2)), txs)
		end
	end
	def check_2(tx, channel, txs) do
		cond do
		(channel == nil) and (tx.data.new != true) ->
				IO.puts("channel doesn't exist yet")
				false
		(channel != nil) and (tx.data.new == true) ->
				IO.puts("channel already exists")
				false
		(channel != nil) and (channel.nonce != 0) ->
				IO.puts("this channel is being closed.")
				false
    true -> true
    end
	end
	def update(tx, d) do
    da = tx.data
		channel = key(da.pub, da.pub2)
    TxUpdate.sym_increment(da.pub, :amount, -da.amount - da.fee, d)
		cb = %Channel{pub: da.pub,
									pub2: da.pub2,
									delay: da.delay}
		if da.create do
			cond do
				d == 1 -> KV.put(da.to, %Account{})
				true -> KV.put(da.to, nil)
			end
		end
		if da.new and d==1 do
			KV.put(channel, cb)
		end
    TxUpdate.sym_increment(channel, String.to_atom(da.to), da.amount, d)
		if da.new and d==-1 do KV.put(channel, nil)	end
		end
end
