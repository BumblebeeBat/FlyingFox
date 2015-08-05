defmodule ChannelBlock do

  #types of bets:
  #1) Hashlock bets go one way if a secret is revealed, and the other ways otherwise.
  #2) judgement bets are locked up unless the judge signs and explanation of who wins how much. The hash of the channel state is recorded so that we can publish the history of the judge's decisions.
  
	defstruct nonce: 0, pub: "", pub2: nil, bets: [], time: 0, delay: 10, fast: false, amount: 0
  #maybe we should stop any channel tx of different types from coexisting in the same block.
	#must specify which hashes are new, and which have existed before. <- NOT a part of the channel state.
	#if one tx is creating a new decision, then don't let any other tx in that same block bet on the same decision.
	def check(tx, txs \\ [], backcheck \\ true) do
    da = tx.data
		channel = KV.get(ToChannel.key(da.pub, da.pub2))
		repeats = txs |> Enum.filter(&(&1.data.__struct__ == tx.data.__struct__ and (&1.data.pub == tx.data.pub and &1.data.pub2 == tx.data.pub2)))
		d = 1
		if channel.pub != da.pub do d = -1 end
    cond do
			channel == nil ->
				IO.puts("channel doesn't exist yet, so you can't spend on it.")
				false
      #run checks on bets and meta.evidence to make sure it only contains valid bets. Make sure they aren't making this element longer than necessary as an attack!
			channel.nonce != 0 and backcheck ->
				IO.puts("a channel block was already published for this channel. ")
				false
			repeats != [] ->
				IO.puts("no repeats")
				false
      not CryptoSign.check_sig2(tx) ->
				IO.puts("bad sig2")
				false
			channel.amount - (da.amount * d) < 0 ->#needs to add up amounts from bets too.
				IO.puts("no counterfeiting: need #{inspect d * da.amount} have #{inspect channel.amount}")
				false
			channel.amount2 + (da.amount * d) < 0 ->#needs to add up amounts from bets too.
				IO.puts("conservation of money: need #{inspect da.amount * d} have #{inspect channel.amount2}")
				false
      #da.secret_hash != nil and da.secret_hash != DetHash.doit(tx.meta.secret) ->
			#	IO.puts("secret does not match")
			#	false
      true -> true
    end
		#fee can be paid by either or both.
	end
	def update(tx, d) do
    da = tx.data
		if da.fast do
      CloseChannel.update(tx, d)
		else
			TxUpdate.sym_replace(channel, :time, 0, KV.get("height"), d)
			TxUpdate.sym_replace(channel, :nonce, 0, da.nonce, d)
			TxUpdate.sym_increment(channel, :amount, da.amount, d)
			TxUpdate.sym_increment(channel, :amount2, -da.amount, d)
		end
	end
end
