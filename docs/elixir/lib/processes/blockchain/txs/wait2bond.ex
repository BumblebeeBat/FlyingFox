defmodule Wait2Bond do
  defstruct nonce: 0, wait_amount: 0, wait_height: 0, pub: ""
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a moving exchange rate. Bond-coins are constantly losing value.
	def check(tx, txs) do
    acc = KV.get(tx.data.pub)
    cond do
			tx.data.wait_amount != acc.wait_amount -> false
			tx.data.wait_height != acc.wait_height -> false
      acc.wait_height > KV.get("height") + Constants.epoch -> false 
      true -> true
    end
	end
	def update(tx, d) do
    da = tx.data
    {a, h} = da.wait_money
    b = KV.get("tot_bonds")
    KV.put("tot_bonds", b + (a * TxUpdate.exchange_rate * d))
    TxUpdate.sym_increment(tx.data.pub, :bond, a * TxUpdate.exchange_rate, d)
    TxUpdate.sym_replace(tx.data.pub, :wait_amount, a, 0, d)
    TxUpdate.sym_replace(tx.data.pub, :wait_height, h, 0, d)
    #TxUpdate.sym_replace(tx.data.pub, :wait, {a, h}, {0, 0}, d)
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a minimum size for purchasing bond-money, priced in spend-money. 
	end
end
