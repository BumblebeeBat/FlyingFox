defmodule CS do
	#odd numbers are one node, even numbers are the other.
	def func1 do
		Cli.new_key
		Cli.status.pubkey
	end
	def func2(key) do#key was the output of fun1
		Cli.spend(key, 500000000)
		Cli.buy_block
		Cli.to_channel(key, 2000)
		Cli.buy_block
		Cli.channel_spend(key, 200)
	end
	def func3(tx, key) do #tx was output of func2
		ChannelManager.accept(tx, 0)
		tx |> Cli.sign |> Cli.pushtx
		Cli.buy_block
		Cli.channel_spend(key, 0)
	end
	def func4(tx, key) do #tx was output of func3
		Cli.buy_block
		Keys.sign(tx) |>	Cli.close_channel_slasher
	end
end
