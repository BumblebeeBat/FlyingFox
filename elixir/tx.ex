defmodule TxUpdate do
  #d is 1 when adding blocks, -1 when removing.
  def sym_replace(pub, key, old, new, d) do
    acc=KV.get(pub)
    cond do
      d==1 -> word=new
      d==-1 -> word=old
    end
    KV.put(pub, Dict.put(acc, key, word)
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
    {pub, _, tx}=tx
    sym_increment(pub, :amount, -tx[:tx][:amount]-tx[:tx][:fee], d)
    sym_increment(tx[:tx][:to], :amount, tx[:tx][:amount], d)
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spend2wait(tx, d) do
    #can only have 1 wait-money at a time.
    h=KV.get("height")
    sym_increment(tx[:pub], :amount, -tx[:tx][:amount]-tx[:tx][:fee], d)
    sym_replace(tx[:pub], :wait, {0,0}, {tx[:tx][:amount], h}, d)
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond(tx, d) do
    {a, h}=tx[:tx][:wait_money]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b+(a*ExchangeRate.main*d))
    sym_increment(tx[:pub], :bond, a*ExchangeRate.main, d)
    sym_replace(tx[:pub], :wait, {a, h}, {0,0}, d)
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in spend-money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the minimum is forced to unbond at that time.
  end
  def bond2spend(tx, d) do
    a=tx[:tx][:amount]
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b-(a*d))
    sym_increment(tx[:pub], :amount, a/ExchangeRate.main-tx[:tx][:fee], d)
    sym_increment(tx[:pub], :bond, -a, d)
    #Users can take their money out of the bond at any time. 
  end
  def sign(tx, d, bond_size) do#0.1% of total bonds is given out as rewards on every block, which changes the exchange rate.
    w=length(tx[:tx][:winners])
    delta=-ExchangeRate.main*bond_size*w
    b=KV.get("tot_bonds")
    KV.put("tot_bonds", b+delta)
    sym_increment(tx[:pub], :bond, delta+((b+delta)*w/1000/Constants.signers_per_block), d)
    #loses some :bond money. total_money
    #Includes hash(entropy_bit+salt).
    #~100 bond-holders are selected every block. A block requires at least 67 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher(tx, d) do
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
  end
  def reveal(tx, d) do
    #lets change old_block somehow so that you cannot reveal the same secret twice.
    sym_append(to_string(tx[:tx][:signed_on]), [:meta, :revealed], tx[:pub], d)
    sym_increment(tx[:pub], :amount, tx[:tx][:amount], d)#during waiting period you are holding cash not bonds.
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
  def tx_update(tx, d, bond_size) do
    case Dict.get(tx[:tx], :type) do
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
end

defmodule VerifyTx do
  def all_positive(addresses) do
    [{h, t}|tail] = addresses
    cond do
      t<0 -> false
      tail==[] -> true
      true -> all_positive(tail)
    end
  end
  def modify_balance(addresses, pub, f) do
    balance=Dict.get(addresses, tx[:pub])
    balance=f.(balance)
    Dict.put(addresses, tx[:pub], balance)
  end
  def lose_key(address, pub, amount, key) do
    f = fn(balance) ->
      Dict.put(balance, key, Dict.get(balance, key)-amount) 
    end
    modify_balance(address, pub, f)
  end
  def lose_cash(address, pub, amount) do
    lose_key(address, pub, amount, :cash)
  end
  def lose_bond(address, pub, amount) do
    lose_key(address, pub, amount, :bond)
  end
  def positive_balances(txs, bond_size, addresses \\ []) do
    cond do
      txs==[] -> all_positive(addresses)
      true -> positive_balances_1(txs, bond_size, addresses)
    end
  end
  def positive_balances_1(txs, bond_size, addresses) do
    [tx|txs]=txs
    pub=tx[:pub]
    type=tx[:tx][:type]
    cond do
      not pub in Dict.keys(addresses) -> 
        acc = KV.get(pub)
        balance = [cash: acc[:amount], bond: acc[:bond]]
      type == :spend ->
        addresses = lose_cash(addresses, pub, tx[:tx][:amount]+tx[:tx][:fee])
      type == :spend2wait ->
        addresses = lose_cash(addresses, pub, tx[:tx][:amount]+tx[:tx][:fee])
      type == :wait2bond ->
        addresses = lose_cash(addresses, pub, tx[:tx][:fee])
      type == :bond2spend ->
        addresses = lose_bond(addresses, pub, tx[:tx][:amount])
        addresses = lose_cash(addresses, pub, tx[:tx][:fee])
      type == :sign ->
        addresses = lose_bond(addresses, pub, bond_size*length(tx[:tx][:winners]))
      type in [:slasher, :reveal, :sign] -> true
      true -> 
        IO.puts("no function with that name")
        true
    end
    positive_balances(txs, bond_size, addresses)
  end
  def check_tx(tx, txs) do
    f = case Dict.get(tx[:tx], :type) do
      :spend ->      spend?
      :spend2wait -> spend2wait?
      :wait2bond ->  wait2bond?
      :bond2spend -> bond2spend?
      :sign ->       sign?
      :slasher ->    slasher?
      :reveal ->     reveal?
      true ->  (fn(a, b) -> false end)
        end
    Sign.verify_tx(tx) and f(tx, txs)
  end
  def check_logic(new, old \\ []) do
    cond do
      length(new) ==0 -> true
      check_tx(hd(new), old) -> 
        check_txs(tl(new),[hd(new)|old])
      true -> false
    end
  end
  def check_txs(txs) do
    spending=Block.being_spent(txs)
    winners=txs_filter(txs, :spend)
    winners=Enum.map(winners, fn(t) -> length(t[:tx][:winners]) end)
    winners=Enum.reduce(winners, 0, &(&1+&2))
    bond_size=spending*3/winners
    positive_balances(txs, bond_size) and check_logic(txs, []) 
  end
  def spend?(tx, txs) do
    #do I have enough money? including repeats? check them all once instead of doing it repeatedly.
    {pub, sig, tx}=tx
    acc=KV.get(pub)
    cond do
      tx[:tx][:to]==nil -> false
      true -> true
    end
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spend2wait?(tx, txs) do
    #do I have enough money? including repeats?
    acc=KV.get(tx[:pub])
    a=tx[:tx][:amount]
    cond do
      {0,0}!=acc[:wait] -> false
      true -> true
    end
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond?(tx, txs) do
    acc=KV.get(tx[:pub])    
    {a, h}=tx[:tx][:wait_money]
    cond do
      {a, h}!=acc[:wait] -> false #{amount, height}
      h>KV.get("height")+epoch -> false #wait 50 blocks
      true -> true
    end
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the mxinimum is forced to unbond at that time.
  end
  def bond2spend?(tx, txs) do
    acc=KV.get(tx[:pub])    
    cond do
      not is_integer(tx[:tx][:amount]) -> false
      not is_integer(tx[:tx][:fee]) -> false      
      (tx[:tx][:amount]) > acc[:bond] -> false
      (tx[:tx][:fee]) > tx[:tx][:amount] -> false
      spenders_count(txs)!=0 -> false
      true -> true
    end
  end
  def winner?(balance, total, seed, pub, j) do#each address gets 200 chances.
    max=HashMath.hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    b=max*Constants.signers_per_block*balance/(200*total)
    a=HashMath.hash2int(DetHash.doit({seed, pub, j}))
    a<b and j>=0 and j<200 and is_integer(j)
  end
  def first_bits(b, s) do
    <<c :: size(s), d :: bitstring>>=b
    c
  end
  def ran_block(height) do
    txs = Block.load_block(height)[:txs]
    cond do
      is_nil(txs) -> 0
      true ->
        txs=Enum.filter(txs, &(&1[:tx][:type]==:reveal))
        txs=Enum.map(txs, &(first_bits(&1[:tx][:secret], length(&1[:tx][:winners]))))
        txs
    end
  end
  def rng do
    h=KV.get("height")
    Enum.map(0..epoch, &(ran_block(h-&1)))
  end
  def sign?(tx, txs) do
    #require hash(enropy+salt)
    #limit 1 sign per block
    acc = KV.get(tx[:pub])
    prev = KV.get("height") 
    tot_bonds = KV.get("tot_bonds")
    prev = prev-1
    prev = KV.get(prev)
    l=Enum.map(tx[:tx][:winners], fn(x)->winner?(acc[:bond], tot_bonds, rng, tx[:pub], x) end)
    l=Enum.reduce(l, true, fn(x, y) -> x and y end)
    #IO.puts "txs #{ inspect txs}"
    m = length(Enum.filter(txs, fn(t)-> t[:tx][:type] == :sign end))
    cond do
      acc[:bond] < Constants.minbond -> false
      not is_binary(tx[:tx][:secret_hash]) -> false
      not l -> false
      length(tx[:tx][:winners])<1 -> false
      m != 0 -> false
      tx[:tx][:prev_hash]!=prev[:hash] -> false
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
    #make sure they cannot do this repeatedly
  end
  def reveal?(tx, txs) do
    b=KV.get(to_string(tx[:tx][:signed_on]))
    old_block=b[:tx]#Block.load_block(tx[:signed_on])
    signed=old_block[:txs] 
    signed = Enum.filter(signed, &(&1[:pub]==tx[:pub])) 
    signed = Enum.filter(signed, &(&1[:tx][:type]==:sign))
    bond_size = old_block[:bond_size]
    winners = Block.txs_filter(old_block[:txs], :sign)
    winners = Enum.filter(winners, &(tx[:pub]==&1[:pub]))
    winners = length(hd(winners)[:tx][:winners])
    cond do
      #make sure old_block hasn't been revealed by this person before
      #secret_hash must match.
      length(signed)==0 ->p 
        IO.puts "0"
        false
      byte_size(tx[:tx][:secret])!=10 -> 
        IO.puts "tx #{inspect tx}"
        IO.puts "1"
        false
      DetHash.doit(tx[:tx][:secret]) != hd(signed)[:tx][:secret_hash] -> 
        IO.puts "2"
        false
      tx[:pub] in b[:meta][:revealed] -> 
        IO.puts "3"
        false
      tx[:tx][:amount]!=bond_size*length(tx[:tx][:winners]) -> 
        IO.puts "4"
        false
      KV.get("height")-epoch>tx[:tx][:signed_on] -> 
        IO.puts "5"
        false
      true -> true
    end
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
  def epoch do 100 end
end
