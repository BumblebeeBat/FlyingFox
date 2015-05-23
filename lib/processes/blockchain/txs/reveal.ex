defmodule Reveal do
  def sign_tx(block, pub) do block.data.txs |> Enum.filter(&(&1.pub == pub))  |> Enum.filter(&(&1.data.__struct__ == :Elixir.SignTx)) end
	def check(tx, txs) do
    old_block = Blockchain.get_block(tx.data.signed_on)
    revealed = txs
    |> Enum.filter(&(&1.data.type == "reveal"))
    |> Enum.filter(&(&1.pub == tx.pub))
    signed = sign_tx(old_block, tx.pub)
    bond_size = old_block.data.bond_size
    blen = bond_size*length(tx.data.winners)
    amount = tx.data.amount
    cond do
      length(revealed) > 0 -> false
      length(signed) == 0 ->
        IO.puts "0"
        false
      byte_size(tx.data.secret) != 10 ->
        IO.puts "1"
        false
      DetHash.doit(tx.data.secret) != hd(signed).data.secret_hash ->
        IO.puts "2"
        false
      tx.pub in old_block.meta.revealed ->
        IO.puts "3"
        false
      amount != blen ->
        IO.puts "4 slfjksd"
        false
      KV.get("height") - Constants.epoch > tx.data.signed_on ->
        IO.puts "5"
        false
      true -> true
    end
    #After you sign, you wait a while, and eventually are able to make this tx. 
    #This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims 
    #the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
	end
	def update(tx, d) do
    old_block = Blockchain.get_block(tx.data.signed_on)
    {reward, delta} = TxUpdate.common(tx, d, old_block, tx.pub)
    TxUpdate.sym_increment(tx.pub, :amount, tx.data.amount + reward + delta, d)#during waiting period you are holding cash not bonds.
	end
end
