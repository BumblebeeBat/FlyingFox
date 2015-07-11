defmodule RG do
	def func1 do
		Cli.new_key
	end
	def func2 do
		#port must be reset every time
		peer = %Peer{ip: "192.241.212.114", port: 6670}
		key = Cli.status(peer).pubkey
		IO.puts("peer key #{inspect key}")
		Cli.spend(key, 1000000)
		Cli.cleanup
		Cli.buy_block
		Cli.to_channel(key, 1000000)
		Cli.buy_block
		Cli.register(peer)
	end
end
