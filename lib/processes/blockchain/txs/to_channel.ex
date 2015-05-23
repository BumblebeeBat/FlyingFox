defmodule ToChannel do
	def check(tx, txs) do
    channel = KV.get(tx.channel)
    cond do
      not tx.data.tx in [:pub, :pub2] -> false
      (channel == nil) and (tx.data.new != true) -> false
      (channel != nil) and (tx.data.new == true) -> false
      true -> true
    end
		#dont allow this any more after a channel_block has been published, or if there is a channel_block tx in the mempool.
	end
	def update(tx, d) do
    da = tx.data
    TxUpdate.sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    cond do
      da.new and d==1 -> KV.put(da.channel, %Channel{pub: da.pub,
                                                     pub2: da.pub2,
                                                    amount: da.amount,
                                                     delay: tx.delay})
      da.new -> KV.put(da.channel, nil)
      true -> TxUpdate.sym_increment(da.channel, da.to, da.amount, d)
    end		
	end
end
