defmodule Spend do
	def check(tx, txs) do
    block = tx.data
    fee = block.fee
    amount = block.amount
    cond do
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
    TxUpdate.sym_increment(tx.pub, :amount, -da.amount - da.fee, d)
    TxUpdate.sym_increment(da.to, :amount, da.amount, d)		
	end
end
