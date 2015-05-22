defmodule CloseChannel do
	def check(tx, txs) do
    #only one per block per channel. be careful.
    channel = KV.get(tx.data.channel)
    case tx.data.type do
      "fast"    -> if VerifyTx.check_sig2(tx) do ChannelBlock.check(tx, txs) end
      "slash"   -> if channel.nonce < tx.data.nonce do ChannelBlock.check(tx, txs) end
      "timeout" -> channel.time < KV.get("height") - channel.delay
    end
	end
	def update(tx, d) do
		da = tx.data
    TxUpdate.sym_increment(da.pub, :amount, da.amount, d)
    TxUpdate.sym_increment(da.pub2, :amount, da.amount2, d)
		#needs to delete the channel
	end
end
