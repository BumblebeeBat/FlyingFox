defmodule TxUpdate do
  #d is 1 when adding blocks, -1 when removing.
  def exchange_rate(n \\ 0) do#how many bonds is a cash worth?
    b=KV.get("height")-n
    :math.pow(1.001, b)
  end  
  def sym_replace(pub, key, old, new, d) do
    acc=KV.get(pub)
    cond do
      d==1 -> word=new
      d==-1 -> word=old
    end
    KV.put(pub, Dict.put(acc, key, word))
  end    
  def sym_increment(pub, key, amount, d) do
    acc=KV.get(pub)
    acc=Dict.put(acc, key, acc[key]+(amount*d))
    KV.put(pub, acc)
  end
  def deep_get(dict, keys) do
    cond do
      keys==[] -> dict
      true -> deep_get(dict[hd(keys)], tl(keys))
    end
  end
  def deep_put(dict, keys, val) do
    cond do
      keys==[] -> val
      true -> Dict.put(dict, hd(keys), deep_put(dict[hd(keys)], tl(keys), val))
    end
  end
  def sym_append(pub, keys, item, d) do
    acc=KV.get(pub)
    a=deep_get(acc, keys)
    cond do
      d ==  1 -> a=a++[item]
      d == -1 -> a=a--[item]
    end
    acc=deep_put(acc, keys, a)
    KV.put(pub, acc)
  end
  def spend(tx, d) do
    sym_increment(tx[:pub], :amount, -tx[:data][:amount]-tx[:data][:fee], d)
    sym_increment(tx[:data][:to], :amount, tx[:data][:amount], d)
  end
  def spend2wait(tx, d) do
    #can only have 1 wait-money at a time.
    h=KV.get("height")
    sym_increment(tx[:pub], :amount, -tx[:data][:amount]-tx[:data][:fee], d)
    sym_replace(tx[:pub], :wait, {0,0}, {tx[:data][:amount], h}, d)
  end
  def wait2bond(tx, d) do
    {a, h}=tx[:data][:wait_money]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b+(a*exchange_rate*d))
    sym_increment(tx[:pub], :bond, a*exchange_rate, d)
    sym_replace(tx[:pub], :wait, {a, h}, {0,0}, d)
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in spend-money. 
  end
  def bond2spend(tx, d) do
    a=tx[:data][:amount]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b-(a*d))
    sym_increment(tx[:pub], :amount, a/exchange_rate-tx[:data][:fee], d)
    sym_increment(tx[:pub], :bond, -a, d)
    #Users can take their money out of the bond at any time. 
  end
  def sign(tx, d, bond_size) do#0.1% of total bonds is given out as rewards on every block, which changes the exchange rate.
    w=length(tx[:data][:winners])
    delta=-exchange_rate*bond_size*w
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b+delta*d)
    sym_increment(tx[:pub], :bond, delta, d)
    #loses some :bond money. total_money
    #The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def common(tx, d, old_block, signer) do
    bond_size = old_block[:data][:bond_size]
    w = length(tx[:data][:winners])
    delta = exchange_rate(old_block[:data][:height])*bond_size*w
    reward = KV.get("tot_bonds")/:math.pow(1.001, Constants.epoch)*w/1000/Constants.signers_per_block
    sym_append(hd(KV.get(to_string(tx[:data][:signed_on]))), [:meta, :revealed], signer, d)
    {reward, delta}
  end
  def slasher(tx, d) do
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
    a=tx[:data]
    old_block = Blockchain.get_block(a[:tx1][:data][:height])
    {reward, delta} = common(tx, d, old_block, a[:tx1][:pub])
    sym_increment(tx[:pub], :amount, tx[:data][:amount]+reward+delta/3, d)
  end
  def reveal(tx, d) do
    old_block = Blockchain.get_block(tx[:data][:signed_on])
    {reward, delta} = common(tx, d, old_block, tx[:pub])
    sym_increment(tx[:pub], :amount, tx[:data][:amount]+reward+delta, d)#during waiting period you are holding cash not bonds.
  end
	def to_channel(tx, d) do
    sym_increment(tx[:pub], :amount, -tx[:data][:amount]-tx[:data][:fee], d)
    cond do
      tx[:new] and d==1 -> KV.put(tx[:channel], [pub: tx[:pub], pub2: tx[:data][:pub2], amount: tx[:data][:amount], amount2: 0, time: 0, nonce: 0, delay: tx[:delay]])
      tx[:new] -> KV.put(tx[:channel], nil)
      true -> sym_increment(tx[:channel], tx[:to], tx[:data][:amount], d)
    end
	end
	def channel_block(tx, d) do
    #need to make a timestamp now for possible refund tx. Also, we should record the nonce for this channel state, so we only update one way.
    sym_replace(tx[:channel], :time, 0, KV.get("height"), d)
    sym_replace(tx[:channel], :nonce, 0, tx[:nonce], d)
    #update state to stop production of to_channel tx. starts timer.
	end
	def close_channel(tx, d) do
    sym_increment(tx[:pub], :amount, tx[:amount], d)
    sym_increment(tx[:pub2], :amount, tx[:amount2], d)
		#needs to delete the channel
	end
  def tx_update(tx, d, bond_size) do
    acc=KV.get(tx[:pub])
    acc=Dict.put(acc, :nonce, acc[:nonce]+d)
    KV.put(tx[:pub], acc)
    case Dict.get(tx[:data], :type) do
      "sign" -> sign(tx, d, bond_size)
      "spend" ->               spend(tx, d)
      "spend2wait" ->     spend2wait(tx, d)
      "wait2bond" ->       wait2bond(tx, d)
      "bond2spend" ->     bond2spend(tx, d)
      "slasher" ->           slasher(tx, d)
      "reveal" ->             reveal(tx, d)
      "to_channel" ->     to_channel(tx, d)
      "channel_block"->channel_block(tx, d)
      "close_channel"->close_channel(tx, d)
      _	-> false			
    end
  end
  def txs_updates(txs, d, bond_size) do
    Enum.map(txs, &(tx_update(&1, d, bond_size)))
  end
end
