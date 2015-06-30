defmodule Spend do
  defstruct nonce: 0, to: "", amount: 0, fee: 10000, create: false, pub: ""
	def check(tx, txs) do
    block = tx.data
    fee = block.fee
    amount = block.amount
    cond do
      fee < Constants.min_tx_fee ->
        IO.puts("fee too low")
        false
      amount+fee > Constants.max_bond ->
        IO.puts("too much money at once")
        false
      true -> true
    end
	end
	def update(tx, d) do
    da = tx.data
		if da.create do
			cond do
				d == 1 -> KV.put(da.to, %Account{})
				true -> KV.put(da.to, nil)
			end
		end
    TxUpdate.sym_increment(tx.data.pub, :amount, -da.amount - da.fee, d)
    TxUpdate.sym_increment(da.to, :amount, da.amount, d)		
	end
end
