defmodule TxUpdate do
  def tx_update(tx, d) do
    case Dict.get(elem(tx, 2), :type) do
      :spend ->      spend(tx, d)
      :spend2wait -> spend2wait(tx, d)
      :wait2bond ->  wait2bond(tx, d)
      :bond2spend -> bond2spend(tx, d)
      :sign ->       sign(tx, d)
      :slasher ->    slasher(tx, d)
      :reveal ->     reveal(tx, d)
      _	->           false			
    end
  end
  def txs_updates(txs, d) do
    Enum.map(txs, &(tx_update(&1, d)))
  end
  def adjust_int(pub, key, amount, d) do
    acc=KV.get(pub)
    acc=Dict.put(acc, key, acc[key]+(amount*d))
    KV.put(pub, acc)
  end
  def spend(tx, d) do
    {pub, sig, tx}=tx
    adjust_int(pub, :amount, -tx[:amount], d)
    adjust_int(tx[:to], :amount, tx[:amount], d)
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spend2wait(tx, d) do
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond(tx, d) do
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the minimum is forced to unbond at that time.
  end
  def bond2spend(tx, d) do
    #Users can take their money out of the bond at any time. 
  end
  def sign(tx, d) do
    #Includes hash(entropy_bit+salt).
    #~64 bond-holders are selected every block. A block requires at least 43 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher(tx, d) do
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
  end
  def reveal(tx, d) do
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
      :spend ->      spend(tx, txs)
      :spend2wait -> spend2wait(tx, txs)
      :wait2bond ->  wait2bond(tx, txs)
      :bond2spend -> bond2spend(tx, txs)
      :sign ->       sign(tx, txs)
      :slasher ->    slasher(tx, txs)
      :reveal ->     reveal(tx, txs)
      _	->           false
    end
  end
  def spend(tx, txs) do
    IO.puts('tx check')
    {pub, sig, tx}=tx
    acc=KV.get(pub)
    IO.puts inspect acc
    a=tx[:amount]
    IO.puts inspect a
    IO.puts inspect tx
    cond do
      a==nil -> false
      not is_integer(a) -> false
      tx[:to]==nil -> false
      true -> true
    end
    #For users to give money to each other. Balances must stay positive. Creator of the tx has a fee which is >=0. The fee pays the creator of the block.
  end
  def spend2wait(tx, txs) do
    false
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
  end
  def wait2bond(tx, txs) do
    false
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in money. 
    #Every several hundred blocks we divide everyones bond-coins in half, and cut the exchange rate in half. That way the numbers dont get too big. Anyone who has less than the minimum is forced to unbond at that time.
  end
  def bond2spend(tx, txs) do
    false
    #Users can take their money out of the bond at any time. 
  end
  def sign(tx, txs) do
    false
    #Includes hash(entropy_bit+salt).
    #~64 bond-holders are selected every block. A block requires at least 43 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher(tx, txs) do
    false
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
  end
  def reveal(tx, txs) do
    false
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
end
