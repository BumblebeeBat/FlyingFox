defmodule CT do
	def func1 do
		Keys.new
		Keys.pubkey
	end
	def func2(key) do#key was the output of fun1
		TxCreator.spend(key, 500000000)
		Cli.buy_block
		TxCreator.to_channel(key, 2000)
		Cli.buy_block
		TxCreator.channel_spend(key, 1000)
	end
	def func3(tx, key) do #tx was output of func2
		Keys.sign(tx) |> Mempool.add_tx
		Cli.buy_block
	end
	def func4(key) do
		Cli.buy_blocks(10)
		:time.sleep(20000)
		TxCreator.close_channel_timeout(key)
	end
end
