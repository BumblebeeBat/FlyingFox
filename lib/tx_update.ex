defmodule TxUpdate do
  #d is 1 when adding blocks, -1 when removing.
  @signers_per_block Application.get_env :flying_fox, :signers_per_block
  @epoch             Application.get_env :flying_fox, :epoch

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
    acc = KV.get(pub)
    acc = Map.put(acc, key, acc[key]+(amount*d))
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
  def spend(tx, d) do
    da = tx.data
    sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    sym_increment(da.to, :amount, da.amount, d)
  end
  def spend2wait(tx, d) do
    #can only have 1 wait-money at a time.
    h = KV.get("height")
    da = tx.data
    sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    sym_replace(tx.pub, :wait, {0,0}, {da.amount, h}, d)
  end
  def wait2bond(tx, d) do
    da = tx.data
    {a, h} = da.wait_money
    b = KV.get("tot_bonds")
    KV.put("tot_bonds", b + (a * exchange_rate * d))
    sym_increment(tx.pub, :bond, a * exchange_rate, d)
    sym_replace(tx.pub, :wait, {a, h}, {0, 0}, d)
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in spend-money. 
  end
  def bond2spend(tx, d) do
    a = tx.data.amount
    b = KV.get("tot_bonds")
    KV.put("tot_bonds", b - (a * d))
    sym_increment(tx.pub, :amount, a / exchange_rate - tx.data.fee, d)
    sym_increment(tx.pub, :bond, -a, d)
    #Users can take their money out of the bond at any time.
  end
  def sign(tx, d, bond_size) do#0.1% of total bonds is given out as rewards on every block, which changes the exchange rate.
    w = length(tx.data.winners)
    delta = -exchange_rate * bond_size * w
    b = KV.get("tot_bonds")
    KV.put("tot_bonds", b + delta * d)
    sym_increment(tx.pub, :bond, delta, d)
    #loses some :bond money. total_money
    #The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def common(tx, d, old_block, signer) do
    bond_size = old_block.data.bond_size
    w = length(tx.data.winners)
    delta = exchange_rate(old_block.data.height) * bond_size * w
    reward = KV.get("tot_bonds") / :math.pow(1.001, @epoch) * w / 1000 / @signers_per_block
    sym_append(hd(KV.get(to_string(tx.data.signed_on))), [:meta, :revealed], signer, d)
    {reward, delta}
  end
  def slasher(tx, d) do
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
    a=tx.data
    old_block = Blockchain.get_block(a.tx1.data.height)
    {reward, delta} = common(tx, d, old_block, a.tx1.pub)
    sym_increment(tx.pub, :amount, tx[:data][:amount] + reward + delta / 3, d)
  end
  def reveal(tx, d) do
    old_block = Blockchain.get_block(tx.data.signed_on)
    {reward, delta} = common(tx, d, old_block, tx.pub)
    sym_increment(tx.pub, :amount, tx.data.amount + reward + delta, d)#during waiting period you are holding cash not bonds.
  end
  def to_channel(tx, d) do
    da = tx.data
    sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    cond do
      da.new and d==1 -> KV.put(da.channel, %Channel{pub: da.pub,
                                                     pub2: da.pub2,
                                                    amount: da.amount,
                                                     delay: tx.delay})
      da.new -> KV.put(da.channel, nil)
      true -> sym_increment(da.channel, da.to, da.amount, d)
    end
  end
  def channel_block(tx, d) do
    #need to make a timestamp now for possible refund tx. Also, we should record the nonce for this channel state, so we only update one way.
    da = tx.data
    sym_replace(da.channel, :time, 0, KV.get("height"), d)
    sym_replace(da.channel, :nonce, 0, da.nonce, d)
    #update state to stop production of to_channel tx. starts timer.
	end
	def close_channel(tx, d) do
    da = tx.data
    sym_increment(da.pub, :amount, da.amount, d)
    sym_increment(da.pub2, :amount, da.amount2, d)
		#needs to delete the channel
  end
  def tx_update(tx, d, bond_size) do
    pub = tx.pub
    acc = KV.get(pub)
    acc = Map.put(acc, :nonce, acc[:nonce] + d)
    KV.put(pub, acc)
    case tx.data.__struct__ do
      :Elixir.SignTx -> sign(tx, d, bond_size)
      :Elixir.SpendTx ->               spend(tx, d)
      :Elixir.Spend2WaitTx ->     spend2wait(tx, d)
      :Elixir.Wait2BondTx ->       wait2bond(tx, d)
      :Elixir.Bond2SpendTx ->     bond2spend(tx, d)
      :Elixir.SlasherTx ->           slasher(tx, d)
      :Elixir.RevealTx ->             reveal(tx, d)
      :Elixir.ToChannelTx ->      to_channel(tx, d)
      :Elixir.ChannelBlockTx-> channel_block(tx, d)
      :Elixir.CloseChannelTx-> close_channel(tx, d)
      _	-> false
    end
  end
  def txs_updates(txs, d, bond_size) do
    Enum.map(txs, &(tx_update(&1, d, bond_size)))
  end
end
