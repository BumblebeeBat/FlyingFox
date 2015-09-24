defmodule Reveal do
  defstruct nonce: 0, signed_on: 0, winners: [], amount: 0, secret: nil, bond_size: 0, pub: ""
  def sign_tx(block, pub) do
		block.data.txs
		|> Enum.filter(&(&1.data.pub == pub))
		|> Enum.filter(&(&1.data.__struct__ == :Elixir.Sign))
	end
	def check(tx, txs) do
    old_block = Blockchain.get_block(tx.data.signed_on)
    revealed = txs
    |> Enum.filter(&(&1.data.__struct__ == :Elixir.Reveal))
    |> Enum.filter(&(&1.data.pub == tx.data.pub))
    signed = sign_tx(old_block, tx.data.pub)
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
      tx.data.pub in old_block.meta.revealed ->
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
    {reward, delta} = TxUpdate.common(tx, d, old_block, tx.data.pub)
    TxUpdate.sym_increment(tx.data.pub, :amount, tx.data.amount + reward + delta, d)#during waiting period you are holding cash not bonds.
	end
end
