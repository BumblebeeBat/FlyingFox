defmodule Spend do
	def check(tx, txs) do
    block = tx.data
    fee = block.fee
    amount = block.amount
    cond do
			tx.data.to == nil and not tx.data.create ->
				IO.puts("need to create this")
				false
			tx.data.to != nil and tx.data.create ->
				IO.puts("already created")
				false
      fee < Constants.min_tx_fee ->
        IO.puts("fee too low")
        false
      amount+fee > Constants.max_bond_block ->
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
    TxUpdate.sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    TxUpdate.sym_increment(da.to, :amount, da.amount, d)		
	end
end
