defmodule CF do
	#odd numbers are one node, even numbers are the other.
	def func1 do
		Keys.new
		Keys.pubkey
	end
	def func2(key) do#key was the output of fun1
		TxCreator.spend(key, 500000000)
		Cli.buy_block
		TxCreator.to_channel(key, 2000)
		Cli.buy_block
		TxCreator.close_channel_fast(key, 1000, 999)
	end
	def func3(tx, key) do #tx was output of func2
		Keys.sign(tx) |> Mempool.add_tx
		Cli.buy_block
	end
end
