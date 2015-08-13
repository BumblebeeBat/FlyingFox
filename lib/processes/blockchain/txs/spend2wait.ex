defmodule Spend2Wait do
  #instead of bonding your money to wait in line to be a validator, this should be a flag on each channel. That way people with money in channels can instantly take it out and validate.
  defstruct nonce: 0, amount: 0, pub: ""
	def check(tx, txs) do
    pub = tx.data.pub
    acc = KV.get(pub)
    cond do
			acc.wait_amount != 0 -> false
			acc.wait_height != 0 -> false
      true -> true
    end
	end
	def update(tx, d) do
    #can only have 1 wait-money at a time.
    h = KV.get("height")
    da = tx.data
    TxUpdate.sym_increment(tx.data.pub, :amount, -da.amount - da.fee, d)
    TxUpdate.sym_replace(tx.data.pub, :wait_amount, 0, da.amount, d)
    TxUpdate.sym_replace(tx.data.pub, :wait_height, 0, h, d)
    #TxUpdate.sym_replace(tx.data.pub, :wait, {0,0}, {da.amount, h}, d)
	end
end
