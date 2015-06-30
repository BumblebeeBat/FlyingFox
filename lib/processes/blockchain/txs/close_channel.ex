defmodule CloseChannel do #maybe this should have a negative fee, since it frees up space on the blockchain.
  defstruct nonce: 0, type: "fast", mount: 0, amount2: 0, pub2: nil, pub: "", secret_hash: nil, bets: [], time: 0, delay: 10
	def check(tx, txs) do
		channel = KV.get(ToChannel.key(tx.data.pub, tx.data.pub2))
    case tx.data.type do
      "fast" ->
				if CryptoSign.check_sig2(tx) do
					ChannelBlock.check(tx, txs, true)
				else
					IO.puts("bad signature close channel")
					false
				end
      "slash" -> if channel.nonce < tx.data.nonce do ChannelBlock.check(tx, txs, true) else false end
      "timeout" -> channel.time < KV.get("height") - channel.delay
    end
	end
	def update(tx, d) do
		x = tx.data
    TxUpdate.sym_increment(x.pub, :amount, x.amount, d)
    TxUpdate.sym_increment(x.pub2, :amount, x.amount2, d)
		#needs to delete the channel
	end
end
