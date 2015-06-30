defmodule Spend2Wait do
  defstruct nonce: 0, amount: 0, pub: ""
	def check(tx, txs) do
    pub = tx.data.pub
    acc = KV.get(pub)
    cond do
      {0,0} != acc.wait -> false
      true -> true
    end
	end
	def update(tx, d) do
    #can only have 1 wait-money at a time.
    h = KV.get("height")
    da = tx.data
    TxUpdate.sym_increment(tx.data.pub, :amount, -da.amount - da.fee, d)
    TxUpdate.sym_replace(tx.data.pub, :wait, {0,0}, {da.amount, h}, d)
	end
end
