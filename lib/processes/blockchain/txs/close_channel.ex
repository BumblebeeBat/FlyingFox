defmodule CloseChannel do #maybe this should have a negative fee, since it frees up space on the blockchain.
  defstruct type: "timeout", amount: 0, amount2: 0, pub2: nil, pub: "", secret_hash: nil, bets: [], channel_block: %ChannelBlock{}, nonce: 0
	def check(tx, txs) do
		channel = KV.get(ToChannel.key(tx.data.pub, tx.data.pub2))
    case tx.data.type do
      "slasher" -> if channel.nonce < tx.data.channel_block.data.nonce do ChannelBlock.check(tx.data.channel_block, txs, false) else false end
      "timeout" -> channel.nonce > 0 and channel.time < KV.get("height") - channel.delay
    end
	end
	def update(tx, d) do
		x = tx.data
    TxUpdate.sym_increment(x.pub, :amount, x.amount, d)
    TxUpdate.sym_increment(x.pub2, :amount, x.amount2, d)
		#needs to delete the channel
	end
end
