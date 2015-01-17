defmodule TxUpdate do
  def tx_update(tx, d, bond_size) do
    case Dict.get(elem(tx, 2), :type) do
      :spend ->      spend(tx, d)
      :spend2wait -> spend2wait(tx, d)
      :wait2bond ->  wait2bond(tx, d)
      :bond2spend -> bond2spend(tx, d)
      :sign ->       sign(tx, d, bond_size)
      :slasher ->    slasher(tx, d)
      :reveal ->     reveal(tx, d)
      _	->           false			
    end
  end
  def txs_updates(txs, d, bond_size) do
    Enum.map(txs, &(tx_update(&1, d, bond_size)))
  end
  def sym_replace(pub, key, old, new, d) do
    acc=KV.get(pub)
    cond do
      d==1 -> acc=Dict.put(acc, key, new)
      d==-1 -> acc=Dict.put(acc, key, old)
    end
    KV.put(pub, acc)
  end    
  def sym_increment(pub, key, amount, d) do
    acc=KV.get(pub)
    acc=Dict.put(acc, key, acc[key]+(amount*d))
    KV.put(pub, acc)
  end
  def sym_append(pub, key, item, d) do#unused
    acc=KV.get(pub)
    cond do
      d==1 -> acc=Dict.put(acc, key, [item|acc[key]])
      d==-1 -> acc=Dict.put(acc, key, tl(acc[key]))
    end
    KV.put(pub, acc)
  end
  def spend(tx, d) do
    {pub, _, tx}=tx
    sym_increment(pub, :amount, -tx[:amount]-tx[:fee], d)
    sym_increment(tx[:to], :amount, tx[:amount], d)
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spend2wait(tx, d) do
    #can only have 1 wait-money at a time.
    {pub, _, tx}=tx
    h=KV.get("height")
    sym_increment(pub, :amount, -tx[:amount]-tx[:fee], d)
    sym_replace(pub, :wait, {0,0}, {tx[:amount], h}, d)
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond(tx, d) do
    {pub, _, tx}=tx
    {a, h}=tx[:wait_money]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b+(a*d))
    sym_increment(pub, :bond, a, d)
    sym_replace(pub, :wait, {a, h}, {0,0}, d)
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the minimum is forced to unbond at that time.
  end
  def bond2spend(tx, d) do
    {pub, _, tx}=tx
    a=tx[:amount]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b-(a*d))
    sym_increment(pub, :amount, a-tx[:fee], d)
    sym_increment(pub, :bond, -a, d)
    #Users can take their money out of the bond at any time. 
  end
  def sign(tx, d, bond_size) do
    {pub, _, tx}=tx
    sym_increment(pub, :bond, -bond_size, d)
    #loses some :bond money. total_money
    #Includes hash(entropy_bit+salt).
    #~64 bond-holders are selected every block. A block requires at least 43 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher(tx, d) do
    {pub, _, tx}=tx
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
  end
  def reveal(tx, d) do
    {pub, _, tx}=tx
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
end

defmodule VerifyTx do
  def test do
    {pub, priv}=Sign.new_key
    tx=[type: :spend]
    tx=Sign.sign_tx(tx, pub, priv)
  end
  def check_txs_helper(new, old) do
    cond do
      new |> length ==0 -> true
      check_tx(hd(new), old) -> 
        check_txs_helper(tl(new),[hd(new)|old])
      true -> false
    end
  end
  def check_txs(txs) do
    check_txs_helper(txs, [])
  end
  def check_tx(tx, txs) do
    cond do
      Sign.verify_tx(tx) -> check_tx_2(tx, txs)
      true -> false
    end
  end
  def check_tx_2(tx, txs) do
    #tx=elem(tx, 2)
    case Dict.get(elem(tx, 2), :type) do
      :spend ->      spend?(tx, txs)
      :spend2wait -> spend2wait?(tx, txs)
      :wait2bond ->  wait2bond?(tx, txs)
      :bond2spend -> bond2spend?(tx, txs)
      :sign ->       sign?(tx, txs)
      :slasher ->    slasher?(tx, txs)
      :reveal ->     reveal?(tx, txs)
      _	->           false
    end
  end
  def spend?(tx, txs) do
    #do I have enough money? including repeats?
    {pub, sig, tx}=tx
    acc=KV.get(pub)
    #IO.puts inspect acc
    a=tx[:amount]
    #IO.puts inspect a
    #IO.puts inspect tx
    cond do
      a==nil -> false
      not is_integer(a) -> false
      tx[:to]==nil -> false
      true -> true
    end
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spenders do [:spend2wait, :spend, :slasher] end
  def spenders_count(txs) do
    length(Enum.filter(txs, &(&1[:type] in spenders)))
  end
  def spend2wait?(tx, txs) do
    #do I have enough money? including repeats?
    {pub, _, tx}=tx
    acc=KV.get(pub)
    a=tx[:amount]
    cond do
      {0,0}!=acc[:wait] -> false
      (tx[:amount] + tx[:fee]) > acc[:amount] -> false
      spenders_count(txs)!=0 -> false
      true -> true
    end
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond?(tx, txs) do
    {pub, _, tx}=tx
    acc=KV.get(pub)    
    {a, h}=tx[:wait_money]
    tx |> inspect |> IO.puts
    acc |> inspect |> IO.puts
    cond do
      {a, h}!=acc[:wait] -> false #{amount, height}
      h>KV.get("height")+50 -> false #wait 50 blocks
      true -> true
    end
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the mxinimum is forced to unbond at that time.
  end
  def bond2spend?(tx, txs) do
    {pub, _, tx}=tx
    acc=KV.get(pub)    
    cond do
      tx[:amount] > acc[:bond] -> false
      (tx[:fee]) > tx[:amount] -> false
      spenders_count(txs)!=0 -> false
      true -> true
    end
  end
  def winner?(balance, total, seed, pub, j) do#each address gets 200 chances.
    max=HashMath.hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    b=max*64*balance/(200*total)
    a=HashMath.hash2int(DetHash.doit({seed, pub, j}))
    a<b and j>=0 and j<200 and is_integer(j)
  end
  def ran_block(height) do
    cond do
      height<1 -> 0
      true ->
        block = KV.get(height)
        IO.puts inspect block
        txs=block[:txs]
        IO.puts inspect txs
        txs=Enum.filter(txs, &(&1[:type]==:reveal))
        txs=Enum.map(txs, &(&1[:entropy]))
        txs
    end
  end
  def rng do
    h=KV.get("height")
    Enum.map(0..20, fn(n) -> ran_block(h-n) end)
  end
  def sign?(tx, txs) do
    #require hash(enropy+salt)
    {pub, _, tx}=tx
    acc = KV.get(pub)
    prev = KV.get("height") 
    tot_bonds = KV.get("tot_bonds")
    prev = prev-1
    prev = KV.get(prev)
    l=Enum.map(tx[:winners], fn(x)->winner?(acc[:bond], tot_bonds, rng, pub, x) end)
    l=Enum.reduce(l, fn(x, y) -> x and y end)
    m = length(Enum.filter(txs, fn(t)-> t[:type] == :sign end))
    IO.puts inspect m
    IO.puts inspect m==0
    cond do
      not is_binary(tx[:secret_hash]) -> 
        IO.puts("a")
        false
      not l -> false
      length(tx[:winners])<1 -> 
        IO.puts("B")
        false
      m != 0 -> 
        IO.puts("C")
        false
      tx[:prev_hash]!=prev[:hash] -> 
        IO.puts("D")
        false
      true -> true
    end
    #Includes hash(entropy_bit+salt).
    #includes hash of previous block.
    #~64 bond-holders are selected every block. A block requires at least 43 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher?(tx, txs) do
    {pub, _, tx}=tx
    false
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
  end
  def reveal?(tx, txs) do
    {pub, _, tx}=tx
    false
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
end
