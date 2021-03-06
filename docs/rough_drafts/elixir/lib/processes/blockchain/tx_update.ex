defmodule TxUpdate do
  def exchange_rate(n \\ 0) do
    #how many bonds is a cash worth?
    b = KV.get("height") - n
    :math.pow(1.001, b)
  end
  def sym_replace(pub, key, old, new, d) do
    acc = KV.get(pub)
    cond do
      d == 1 -> word = new
      d == -1 -> word = old
    end
    KV.put(pub, Map.put(acc, key, word))
  end
  def sym_increment(pub, key, amount, d) do
		acc = KV.get(pub)#if it doesn't exist yet, we need to add it.
    acc = Map.put(acc, key, Map.get(acc, key)+(amount*d))#amount is nil?
    KV.put(pub, acc)
  end
  def deep_get(dict, keys) do
    cond do
      keys == [] -> dict
      true -> deep_get(Map.get(dict, hd(keys)), tl(keys))
    end
  end
  def deep_put(dict, keys, val) do
    cond do
      keys == [] -> val
      true -> Map.put(dict, hd(keys), deep_put(Map.get(dict, hd(keys)), tl(keys), val))
    end
  end
  def sym_append(pub, keys, item, d) do
    acc = KV.get(pub)
    a = deep_get(acc, keys)
    cond do
      d ==  1 -> a = a ++ [item]
      d == -1 -> a = a -- [item]
    end
    acc = deep_put(acc, keys, a)
    KV.put(pub, acc)
  end
  def common(tx, d, old_block, signer) do
    bond_size = old_block.data.bond_size
    w = length(tx.data.winners)
    delta = exchange_rate(old_block.data.height) * bond_size * w
    reward = KV.get("tot_bonds") / :math.pow(1.001, Constants.epoch) * w / 1000 / Constants.signers_per_block
    sym_append(hd(KV.get(to_string(tx.data.signed_on))), [:meta, :revealed], signer, d)
    {reward, delta}
  end
  def tx_update(tx, d, bond_size) do
    pub = tx.data.pub
    acc = KV.get(pub)
    acc = Map.put(acc, :nonce, acc.nonce + d)
    KV.put(pub, acc)
    case tx.data.__struct__ do
      :Elixir.Sign ->                 Sign.update(tx, d, bond_size)
      :Elixir.Spend ->               Spend.update(tx, d)
      :Elixir.Spend2Wait ->     Spend2Wait.update(tx, d)
      :Elixir.Wait2Bond ->       Wait2Bond.update(tx, d)
      :Elixir.Bond2Spend ->     Bond2Spend.update(tx, d)
      :Elixir.Slasher ->           Slasher.update(tx, d)
      :Elixir.Reveal ->             Reveal.update(tx, d)
      :Elixir.ToChannel ->       ToChannel.update(tx, d)
      :Elixir.ChannelBlock->  ChannelBlock.update(tx, d)
      :Elixir.CloseChannel->  CloseChannel.update(tx, d)
			:Elixir.Oracle ->             Oracle.update(tx, d)
			:Elixir.Judgement ->       Judgement.update(tx, d)
			:Elixir.Win ->                   Win.update(tx, d)
      _	-> false
    end
  end
  def txs_updates(txs, d, bond_size) do
    Enum.map(txs, &(tx_update(&1, d, bond_size)))
  end
end
