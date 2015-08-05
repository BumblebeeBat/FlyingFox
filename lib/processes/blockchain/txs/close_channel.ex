defmodule CloseChannel do #maybe this should have a negative fee, since it frees up space on the blockchain.
  defstruct type: "timeout", amount: 0, pub2: nil, pub: "", bets: [], channel_block: %ChannelBlock{}, nonce: 0
	def check(tx, txs) do
		channel = KV.get(ToChannel.key(tx.data.pub, tx.data.pub2))
    case tx.data.type do
      "slasher" -> if channel.nonce < tx.data.channel_block.data.nonce do ChannelBlock.check(tx.data.channel_block, txs, false) else false end
      "timeout" -> channel.nonce > 0 and channel.time < KV.get("height") - channel.delay
    end
	end
  def settle_bet(pub, pub2, bet, evidence) do
    # bet = %{amount: 100000, root: fioj892jf92fejwf, default: 0.5} #default is the portion of the money that goes to pub if the judge never signs, or if the secret is never revealed.
    #sender = "pub"
    #if sender == bet.to do sender = "pub2" end
    #sender = tx[sender]
    keys = Dict.keys(evidence)
    if bet.root in keys do
      reciever = tx[bet.to]
      ev = evidence[bet.root]
      settle_bet_2(pub, pub2, bet, ev)
    else
		  TxUpdate.sym_increment(pub, :amount, bet.amount * bet.default, d)
		  TxUpdate.sym_increment(pub2, :amount, bet.amount * (1 - bet.default), d)
    end
  end
  def settle_bet_2(pub, pub2, bet, ev) do
    # example ev's
    # ev = %{type: hashlock, secret: jf920fj9032f43j4j}
    # ev = %{type: judgement, outcome: 0.1, sig: fj920jf2302929i209, bet: %{bet_hash: 932r0j9f0j24f9490f, judge: pubkey}} 10% of money gets transfered in this case
    cond do
      ev.type == "hashlock" and DetHash.doit(ev.secret) == bet.root ->
		    TxUpdate.sym_increment(pub, :amount, bet.amount * (1 - bet.default), d)
		    TxUpdate.sym_increment(pub2, :amount, bet.amount * bet.default, d)
      ev.type == "judgement" and DetHash.doit(ev.bet) == bet.root and CryptoSign.verify(DetHash.doit(%{outcome: ev.outcome, bet: bet.root}), ev.sig, ev.judge) ->
		    TxUpdate.sym_increment(pub, :amount, bet.amount * ev.outcome, d)
		    TxUpdate.sym_increment(pub2, :amount, bet.amount * (1 - ev.outcome), d)
      true ->
		    TxUpdate.sym_increment(pub, :amount, bet.amount * bet.default, d)
		    TxUpdate.sym_increment(pub2, :amount, bet.amount * (1 - bet.default), d)
    end
  end
	def update(tx, d) do
    da = tx.data
		channel = ToChannel.key(da.pub, da.pub2)
		current = KV.get(channel)
		TxUpdate.sym_increment(da.pub, :amount, current.amount+da.amount, d)
		TxUpdate.sym_increment(da.pub2, :amount, current.amount2-da.amount, d)
    Enum.map(da.bets, &(settle_bet(da.pub, da.pub2, &1, tx.meta.evidence)))
    #needs to delete the channel
	end
end
