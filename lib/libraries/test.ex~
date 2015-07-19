defmodule Test do
	#each of the 2-letter functions is a test for the system. To run tests, start 2 flying fox nodes on the same computer. Then run any of these 2-letter functions from either computer.
	# warning, if you run tests on the main-chain, you will spend money.
	def peer do
		Cli.all_peers
		|> Enum.filter(&(&1.port != Port.port))
		|> Enum.filter(&(&1.ip == "localhost"))
		|> Enum.sort(&(&1.height > &2.height))
		|> hd
	end
	def channel_common do
		p = peer
		Cli.new_key(p)
		key = Cli.status(p).pubkey
		Cli.spend(key, 500000000)
		Cli.buy_block
		Cli.to_channel(key, 5000)
		Cli.buy_block
		{p, key}
	end
	def cf do
		{p, key}  = channel_common
		Cli.channel_spend(key, 2000)
		Cli.close_channel_fast(key)
		|> Cli.sign(p)
		|> Cli.pushtx
		Cli.buy_block
	end
	def ct do
		{p, key}  = channel_common
		Cli.channel_spend(key, 2000)
		|> Cli.sign(p)
		|> Cli.pushtx
		Cli.buy_blocks(12)
		:timer.sleep(24000)
		Cli.close_channel_timeout(key)
		Cli.buy_block
	end
	def cs do
		{p, key}  = channel_common
		tx = Cli.channel_spend(key, 2000)
		Cli.channel_accept(tx, 0, p)
		tx
		|> Cli.sign(p)
		|> Cli.pushtx(p)
		Cli.buy_block(p)
		:timer.sleep(2000)
		tx = Cli.channel_spend(Keys.pubkey, 2000, p)
		Cli.buy_block
		tx
		|> Keys.sign
		|> Cli.close_channel_slasher
	end
	def rg do
		p = peer
		Cli.new_key(p)
		key = Cli.status(p).pubkey
		Cli.spend(key, 1000000)
		Cli.cleanup
		Cli.buy_block
		Cli.to_channel(key, 2000000)
		Cli.buy_block
		:timer.sleep(5000)
		Cli.register(p)
	end
	def da do
		rg
		Cli.delete_account(peer)
	end
	def sm do
		p = peer
		rg
		Cli.send_message(Cli.status(p).pubkey, "example message", p)
		#Cli.inbox_size(p)
	end
	def dm do
		sm
		p = peer
		a = Cli.inbox_size(p)
		IO.puts("dm size #{inspect a}")
		Cli.delete_message(0, p)
		a = Cli.inbox_size(p)
		IO.puts("dm size #{inspect a}")
	end
	#def rm do
	#	sm
	#	Cli.read_message(0, peer)
	#end
	def is do
		rg
		Cli.inbox_size(peer)
	end
end
