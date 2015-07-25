defmodule Spend do
  defstruct nonce: 0, to: "", amount: 0, fee: 10000, create: false, pub: ""
	def check(tx, txs) do
    block = tx.data
    fee = block.fee
    amount = block.amount
		acc = KV.get(tx.data.to)
    cond do
			amount < 0 ->
				IO.puts("can't spend negative")
				false
			acc == nil and not tx.data.create ->
				IO.puts("this account doesn't exist yes")
				false
			acc != nil and tx.data.create ->
				IO.puts("this account already exists")
				false
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
