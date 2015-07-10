defmodule CT do
	def func1 do
		Cli.new_key
		Cli.status.pubkey
	end
	def func2(key) do#key was the output of fun1
		Cli.spend(key, 500000000)
		Cli.buy_block
		Cli.to_channel(key, 5000)
		Cli.buy_block
		Cli.channel_spend(key, 200)
	end
	def func3(tx, key) do #tx was output of func2
		Keys.sign(tx) |> Cli.pushtx
		Cli.buy_block
	end
	def func4(key) do
		Cli.buy_blocks(11)
		:timer.sleep(22000)
		Cli.close_channel_timeout(key)
	end
end
