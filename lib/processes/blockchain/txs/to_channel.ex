defmodule ToChannel do
	def key(a, b) do
		cond do
			a > b -> a <> b
			true -> b <> a
		end
	end
	def check(tx, txs) do
    channel = KV.get(key(tx.pub, tx.pub2))
    #channel = KV.get(tx.channel)
    cond do
      not tx.data.tx in [:pub, :pub2] -> false
			KV.get(tx.pub) == nil -> false
			KV.get(tx.pub2) == nil -> false
			(channel == nil) and (tx.data.new != true) -> false
      (channel != nil) and (tx.data.new == true) -> false
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
    cond do
      da.new and d==1 -> KV.put(channel,
																%Channel{pub: da.pub,
                                         pub2: da.pub2,
                                         amount: da.amount,
                                         delay: da.delay})
      da.new -> KV.put(channel, nil)#this should be in a different key value store. Storing different things in the same place is dumb.
      true -> TxUpdate.sym_increment(da.channel, da.to, da.amount, d)
    end		
	end
end
