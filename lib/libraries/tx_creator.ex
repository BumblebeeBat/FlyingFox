defmodule TxCreator do
  def nonce(pub) do
    a=Mempool.txs
    |> Enum.filter(fn(tx) -> tx.data.pub == pub end)
    |> length
    a+KV.get(pub).nonce
  end
	def broadcast(tx) do
		tx
		|> Keys.sign
		|> Mempool.add_tx
	end
  def spend(to, amount) do
    pub = Keys.pubkey
    balance = KV.get(pub).amount
		if is_binary(amount) do amount = String.to_integer(amount) end
		fee = 10000
    if balance < (amount + fee) do
			IO.puts("warning, you cannot afford to spend this tx, so it wont be valid")
		end
		to_now = KV.get(to)
		create = (to_now == nil)
    %Spend{to: to, amount: amount, nonce: nonce(pub), fee: fee, create: create, pub: pub} 
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
      w = Enum.filter(0..Constants.chances_per_address, fn(x) -> Sign.winner?(acc.bond, tot_bonds, Sign.rng(prev_hash), pub, x) end) 
      h = KV.get("height") + 1
      ran = KV.get("secret #{inspect h}")
      if ran == nil do
        ran = :crypto.rand_bytes(10)
        KV.put("secret #{inspect h}", ran)
      end
      secret = DetHash.doit(ran)
      %Sign{prev_hash: prev_hash, winners: w, secret_hash: secret, nonce: nonce(pub), height: h-1, pub: pub}
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
      |> Enum.filter(&(&1.data.__struct__ == :Elixir.Sign)) 
      |> Enum.filter(&(&1.data.pub == pub)) 
      |> hd
    w=old_tx.data.winners
    bond_size=old_block.data.bond_size
    secret = KV.get("secret #{inspect h}")
    if secret != nil do
      %Reveal{signed_on: h, 
              winners: w, 
              amount: length(w)*bond_size, 
              secret: KV.get("secret #{inspect h}"), 
              nonce: nonce(pub), pub: pub}
      |> broadcast
    end
  end
  def slasher(tx1, tx2) do
  end
  def to_channel(other, amount, delay \\ 10) do
		if KV.get(other) == nil do
			IO.puts("your partner doesn't exist yet, so this to_channel tx probably wont work")
		end
		if is_binary(amount) do amount = String.to_integer(amount) end
		is_ch = KV.get(ToChannel.key(Keys.pubkey, other))
		new = (is_ch == nil)
		pub2_now = KV.get(other)
		create = (pub2_now == nil)
		tx = %ToChannel{amount: amount, new: new,	to: "amount", pub: Keys.pubkey, pub2: other, nonce: nonce(Keys.pubkey), create: create}
		if new do
			tx = %{tx | delay: delay}
		else
			if is_ch.pub2 == Keys.pubkey do tx = %{tx | to: "amount2"} end
		end
		tx |> broadcast
		#the channel should be updated in the channel manager.
  end
  def close_channel_fast(other) do
		#needs  channel manager top block
		c = ToChannel.key(Keys.pubkey, other)
		if KV.get(c) == nil do
			IO.puts("this channel doesn't exist yet, so you cannot close it.")
		end
		cb = ChannelManager.get(other)
		|> ChannelManager.top_block
		%{cb.data | fast: true}
		#once it is accepted into the bockchain, we should delete the channel from the channel manager
    |> Keys.sign
  end
  def close_channel_timeout(other) do
		c = ToChannel.key(Keys.pubkey, other)
		if KV.get(c) == nil do
			IO.puts("this channel doesn't exist yet, so you cannot close it.")
		end
		c = KV.get(c)
		if not(c.nonce > 0 and c.time < (KV.get("height") - c.delay)) do
			IO.puts("you need to wait longer. #{inspect c.time - (KV.get("height") - c.delay) + 1}")
		end
		IO.puts("channel on blockchain #{inspect c}")
		cb = %CloseChannel{pub: c.pub, pub2: c.pub2, type: "timeout", nonce: nonce(Keys.pubkey)}
    cb |> broadcast
  end
  def close_channel_slasher(tx) do
		other = [tx.data.pub, tx.data.pub2] |> Enum.filter(&(&1 != Keys.pubkey)) |> hd
		
		c = ToChannel.key(Keys.pubkey, other)
		if KV.get(c) == nil do
			IO.puts("this channel doesn't exist yet, so you cannot close it.")
		end
		c = KV.get(c)
		IO.puts("channel on blockchain #{inspect c}")
		cb = %CloseChannel{pub: c.pub, pub2: c.pub2, type: "slasher", channel_block: tx, nonce: nonce(Keys.pubkey)}
    cb |> broadcast
  end
end
