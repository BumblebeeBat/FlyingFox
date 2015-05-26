defmodule TxCreator do
  def nonce(pub) do
    a=Mempool.txs
    |> Enum.filter(fn(tx) -> tx.pub == pub end)
    |> length
    a+KV.get(pub).nonce
  end
  def spend(amount, to) do
    pub = Keys.pubkey
    balance = KV.get(pub).amount
    if balance<amount do IO.puts("warning, you cannot afford to spend this tx, so it wont be valid") end
    %SpendTx{to: to, amount: amount, nonce: nonce(pub), fee: 10000} 
    |> Keys.sign 
    |> Mempool.add_tx
  end
  def sign do
		IO.puts("tx creator sign")
    pub = Keys.pubkey
    acc = KV.get(pub)
    if acc.bond > Constants.min_bond do
      h=KV.get("height")
      if h<1 do prev_hash=nil else
        prev_hash = Blockchain.blockhash(Blockchain.get_block(h))
      end
      tot_bonds = KV.get("tot_bonds")
      w= Enum.filter(0..Constants.chances_per_address, fn(x) -> SignTransaction.winner?(acc.bond, tot_bonds, SignTransaction.rng(prev_hash), pub, x) end) 
      h = KV.get("height") + 1
      ran = KV.get("secret #{inspect h}")
      if ran == nil do
        ran = :crypto.rand_bytes(10)
        KV.put("secret #{inspect h}", ran)
      end
      secret = DetHash.doit(ran)
			IO.puts("created sign tx #{inspect %SignTx{prev_hash: prev_hash, winners: w, secret_hash: secret, nonce: nonce(pub), height: h-1}}")
      %SignTx{prev_hash: prev_hash, winners: w, secret_hash: secret, nonce: nonce(pub), height: h-1}
      |> Keys.sign
      |> Mempool.add_tx
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
      %RevealTx{signed_on: h, 
                winners: w, 
                amount: length(w)*bond_size, 
                secret: KV.get("secret #{inspect h}"), 
                nonce: nonce(pub)}
      |> Keys.sign
      |> Mempool.add_tx
    end
  end
  def slasher(tx1, tx2) do
  end
  def to_channel do
    %ToChannelTx{}
  end
  def channel_block do
    %ChannelBlockTx{}
  end
  def close_channel do
    %CloseChannelTx{}
  end
end
