defmodule ToChannel do
  defstruct nonce: 0, to: "pub", amount: 0, amount2: 0, new: false, delay: 100, fee: 10000, pub: "", pub2: ""
	def key(a, b) do
		cond do
			a > b -> a <> b
			true -> b <> a
		end
	end
	def check(tx, txs) do
    cond do
      not tx.data.to in ["pub", "pub2"] ->
				IO.puts("bad to")
				false
			tx.data.pub == nil ->
				IO.puts("nil pub #{inspect tx.data.pub}")
				false
			tx.data.pub2 == nil ->
				IO.puts("nil pub 2 #{inspect tx.data.pub2}")
				IO.puts("tx #{inspect tx}")
				false
			KV.get(tx.data.pub) == nil ->
				IO.puts("account hasn't been registered #{inspect tx.data.pub}")
				false
			KV.get(tx.data.pub2) == nil ->
				IO.puts("account hasn't been registered #{inspect tx.data.pub2}")
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
		#dont allow this any more after a channel_block has been published, or if there is a channel_block tx in the mempool.
	end
	def update(tx, d) do
    da = tx.data
		IO.puts("da.pub #{inspect da.pub}")
		IO.puts("KV get da.pub #{inspect KV.get(da.pub)}")
    TxUpdate.sym_increment(da.pub, :amount, -da.amount - da.fee, d)
		channel = key(da.pub, da.pub2)
		cb = %ChannelBlock{pub: da.pub,
											 pub2: da.pub2,
											 amount: da.amount,
											 amount2: da.amount2,
											 delay: da.delay}
    cond do
      da.new and d==1 -> KV.put(channel, cb)
      da.new -> KV.put(channel, nil)
      true -> TxUpdate.sym_increment(da.channel, da.to, da.amount, d)
    end
		f = false
		if da.pub == Keys.pubkey do
			other = da.pub2
			f = true
		end
		if da.pub2 == Keys.pubkey do
			other = da.pub
			f = true
		end
		if f do
			c = ChannelManager.get(other)
			if c == nil do c = cb	end
			if tx.data.to == da.pub do
				c = %{c | amount: c.amount + (d * da.amount)}
			else
				c = %{c | amount2: c.amount2 + (d * da.amount)}
			end
			ChannelManager.update(other, c)
		end
	end
end
