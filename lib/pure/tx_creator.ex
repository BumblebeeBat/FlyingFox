defmodule TxCreator do
  def nonce(pub) do
    a=Mempool.txs
    |> Enum.filter(fn(tx) -> tx.pub == pub end)
    |> length
    a+KV.get(pub).nonce
  end
	def broadcast(tx) do tx |> Keys.sign |> Mempool.add_tx end
  def spend(amount, to) do
    pub = Keys.pubkey
    balance = KV.get(pub).amount
		fee = 10000
    if balance < (amount + fee) do
			IO.puts("warning, you cannot afford to spend this tx, so it wont be valid")
		end
		to_now = KV.get(to)
		create = (to_now == nil)
    %Spend{to: to, amount: amount, nonce: nonce(pub), fee: fee, create: create} 
    |> broadcast
  end
  def sign do
    pub = Keys.pubkey
    acc = KV.get(pub)
    if acc.bond > Constants.min_bond do
      h=KV.get("height")
      if h<1 do prev_hash=nil else
        prev_hash = Blockchain.blockhash(Blockchain.get_block(h))
      end
      tot_bonds = KV.get("tot_bonds")
      w= Enum.filter(0..Constants.chances_per_address, fn(x) -> Sign.winner?(acc.bond, tot_bonds, Sign.rng(prev_hash), pub, x) end) 
      h = KV.get("height") + 1
      ran = KV.get("secret #{inspect h}")
      if ran == nil do
        ran = :crypto.rand_bytes(10)
        KV.put("secret #{inspect h}", ran)
      end
      secret = DetHash.doit(ran)
      %Sign{prev_hash: prev_hash, winners: w, secret_hash: secret, nonce: nonce(pub), height: h-1}
      |> broadcast
    end
  end
  def reveal do
    h = KV.get("height") - Constants.epoch
    cond do
      h < 2 -> nil
      true -> reveal_2(h)
    end
  end
  def reveal_2(h) do
    pub = Keys.pubkey
    old_block=Blockchain.get_block(h)
    old_tx = old_block.data.txs 
      |> Enum.filter(&(&1.data.type == "sign")) 
      |> Enum.filter(&(&1.pub == pub)) 
      |> hd
    w=old_tx.data.winners
    bond_size=old_block.data.bond_size
    secret = KV.get("secret #{inspect h}")
    if secret != nil do
      %Reveal{signed_on: h, 
              winners: w, 
              amount: length(w)*bond_size, 
              secret: KV.get("secret #{inspect h}"), 
              nonce: nonce(pub)}
      |> broadcast
    end
  end
  def slasher(tx1, tx2) do
  end
  def to_channel(other, amount, amount2 \\ 0, delay \\ 10) do
		#pub is "pub" or "pub2"
		if KV.get(other) == nil do
			IO.puts("your partner doesn't exist yet, so this probably wont work")
		end
		is_ch = KV.get(ToChannel.key(Keys.pubkey, other))
		new = (is_ch == nil)
		tx = %ToChannel{
					amount: amount,
					amount: amount2,
					new: new}
		if new do
			tx2 = %{tx | pub: Keys.pubkey, pub2: other, delay: delay, nonce: nonce(Keys.pubkey)}
		else
			cond do
				is_ch.pub == Keys.pub  -> tx2 = %{ tx | to: "pub"}
				is_ch.pub2 == Keys.pub -> tx2 = %{ tx | to: "pub2"}
				true -> IO.puts("that isn't your channel")
			end
		end
		tx2 |> broadcast
  end
  def channel_block(other, amount, amount2, bets \\ [], nonce \\ 1) do
		c = ToChannel.key(Keys.pubkey, other)
		if KV.get(c) == nil do
			IO.puts("this channel doesn't exist yet, so you cannot close it.")
		end
		#make sure that amount+amont2+bets is less than or equal to the total money.
		#example bets: ["hash1": amount1, "hash2": amount2]
    %ChannelBlock{channel: c, amount: amount, amount2: amount2, bets: bets, nonce: nonce}
		#|> broadcast
  end
  def close_channel do
    %CloseChannel{}
		|> broadcast
  end
end
