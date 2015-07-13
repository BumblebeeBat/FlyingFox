defmodule CF do
	#odd numbers are one node, even numbers are the other.
	def peer(port) do %Peer{ip: "localhost", port: port} end
	def func1 do
		Cli.new_key
		#Cli.status.pubkey
		Port.port
	end
	def func2(port) do#key was the output of fun1
		p = peer(port)
		key = Cli.status(p).pubkey
		Cli.spend(key, 500000000)
		Cli.buy_block
		Cli.to_channel(key, 5000)
		Cli.buy_block
		Cli.channel_spend(key, 200)
		Cli.close_channel_fast(key)
	end
	def func3(tx, key) do #tx was output of func2
		tx
		|> Cli.sign
		|> Cli.pushtx
		Cli.buy_block
	end
end
