defmodule ChannelBlock do
  #maybe we should stop any channel tx of different types from coexisting in the same block.
	#must specify which hashes are new, and which have existed before. <- NOT a part of the channel state.
	#if one tx is creating a new decision, then don't let any other tx in that same block bet on the same decision.
	def check(tx, txs) do
    da = tx.data
    channel = KV.get(da.channel)
		b = tx.bets |> Enum.map(fn(x) -> x.amount end)
		c = b |> Enum.reduce(0, &(&1+&2))
		bool = b |> Enum.map(fn(x) -> x >= 0 end) |> Enum.reduce(&(&1 and &2))
    cond do
			not bool ->
				IO.puts("no negative money")
				false
      not VerifyTx.check_sig2(tx) -> false
      da.amount + da.amount2 + c > channel[da.pub] + channel[da.pub2] ->
				IO.puts("no counterfeiting")
				false
      da.secret_hash != nil and da.secret_hash != DetHash.doit(tx.meta.secret) ->
				IO.puts("secret does not match")
				false
      true -> true
    end
		#must contain the entire current state of the channel.
		#fee can be paid by either or both.

	end
	def update(tx, d) do
    #need to make a timestamp now for possible refund tx. Also, we should record the nonce for this channel state, so we only update one way.
    da = tx.data
    TxUpdate.sym_replace(da.channel, :time, 0, KV.get("height"), d)
    TxUpdate.sym_replace(da.channel, :nonce, 0, da.nonce, d)
		#
    #update state to stop production of to_channel tx. starts timer.
	end
end
