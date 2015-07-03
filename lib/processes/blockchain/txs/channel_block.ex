defmodule ChannelBlock do
	#old amount is a critical flaw. That means that a to_channel tx can be used to invalidate an existing channel tx. :(
	#It should be modified so that "amount" means how much we want to change the channel by, no the final amount.

	defstruct nonce: 0, pub: "", pub2: nil, secret_hash: nil, bets: [], time: 0, delay: 10, fast: false, amount: 0
  #maybe we should stop any channel tx of different types from coexisting in the same block.
	#must specify which hashes are new, and which have existed before. <- NOT a part of the channel state.
	#if one tx is creating a new decision, then don't let any other tx in that same block bet on the same decision.
	def check(tx, txs, backcheck \\ true) do
    da = tx.data
		channel = KV.get(ToChannel.key(da.pub, da.pub2))
		#b = da.bets |> Enum.map(fn(x) -> x.amount end)
		#c = b |> Enum.reduce(0, &(&1+&2))
		#bool = b
		|> Enum.map(fn(x) -> x >= 0 end)
		|> Enum.reduce(true, &(&1 and &2))
		repeats = txs |> Enum.filter(&(&1.data.__struct__ == tx.data.__struct__ and (&1.data.pub == tx.data.pub and &1.data.pub2 == tx.data.pub2)))
    cond do
			da.bets != [] ->
				IO.puts("bets not yet programmed")
				false
			channel.nonce != 0 and backcheck ->
				IO.puts("a channel block was already published for this channel. ")
				false
			repeats != [] ->
				IO.puts("no repeats")
				false
			not bool ->
				IO.puts("no negative money")
				false
      not CryptoSign.check_sig2(tx) ->
				IO.puts("bad sig2")
				false
			channel.amount + da.amount < 0 ->
				IO.puts("no counterfeiting")
				false
			channel.amount2 - da.amount < 0 ->
				IO.puts("conservation of money")
				false
      #da.amount + da.amount2 + c > channel.amount + channel.amount2 -> 
			#	IO.puts("no counterfeiting")
			#	false
      da.secret_hash != nil and da.secret_hash != DetHash.doit(tx.meta.secret) ->
				IO.puts("secret does not match")
				false
			#da.old_amount != channel.amount ->
			#	IO.puts("wrong old amount")
			#	false
			#da.old_amount2 != channel.amount2 ->
			#	IO.puts("wrong old amount2")
			#	false
      true -> true
    end
		#must contain the entire current state of the channel.
		#fee can be paid by either or both.

	end
	def update(tx, d) do
    #need to make a timestamp now for possible refund tx. Also, we should record the nonce for this channel state, so we only update one way.
    da = tx.data
		channel = ToChannel.key(da.pub, da.pub2)
		if da.fast do
			TxUpdate.sym_increment(da.pub, :amount, channel.amount+da.amount, d)
			TxUpdate.sym_increment(da.pub2, :amount, channel.amount2-da.amount, d)
			#should delete the state.
		else
			TxUpdate.sym_replace(channel, :time, 0, KV.get("height"), d)
			TxUpdate.sym_replace(channel, :nonce, 0, da.nonce, d)
			TxUpdate.sym_increment(channel, :amount, channel.data.amount, d)
			TxUpdate.sym_increment(channel, :amount2, -channel.data.amount, d)
			#update state to stop production of to_channel tx. starts timer.
		end
	end
end
