defmodule Slasher do
  defstruct nonce: 0, tx1: nil, tx2: nil, signed_on: 0
	def check(tx, txs) do
		old_block = Blockchain.get_block(tx.data.signed_on)
    tx1 = tx.data.tx1
    tx2 = tx.data.tx2
		check_sig = fn(tx) -> CryptoSign.verify_tx(tx) end
    cond do
      tx.data.tx1.pub in old_block.meta.revealed ->
        IO.puts "slasher reuse"
        false
      tx1.data.prev_hash == tx2.data.prev_hash ->
        IO.puts("same tx_hash")
        false
      tx1.data.height != tx2.data.height ->
        IO.puts("different height")
        false
      not check_sig.(tx1) ->
        IO.puts("unsigned")
        false
      not check_sig.(tx2) ->
        IO.puts("unsigned 2")
        false
    end
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
	end
	def update(tx, d) do
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
    a=tx.data
    old_block = Blockchain.get_block(a.tx1.data.height)
    {reward, delta} = TxUpdate.common(tx, d, old_block, a.tx1.pub)
    TxUpdate.sym_increment(tx.pub, :amount, tx[:data][:amount] + reward + delta / 3, d)
	end
end
