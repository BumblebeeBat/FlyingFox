defmodule Test do
	#each of the 2-letter functions is a test for the system. To run tests, start 2 flying fox nodes on the same computer. Then run any of these 2-letter functions from either computer.
	# warning, if you run tests on the main-chain, you will spend money.
	def peer_helper do
		Cli.all_peers
		|> Enum.filter(&(&1.port != Port.port))
		|> Enum.filter(&(&1.ip == "localhost"))
		|> Enum.sort(&(&1.height > &2.height))
	end		
	def peer do peer_helper |> hd	end
	def peer2 do peer_helper |> tl |> hd end
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
	#These tests need 2 nodes
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
	#these tests need 3 nodes
	def ms do
		delay = 5000
		p1 = peer #central node
		p2 = peer2
		Cli.new_key(p1)
		Cli.new_key(p2)
		key1 = Cli.status(p1).pubkey
		key2 = Cli.status(p2).pubkey
		Cli.spend(key1, 10000000)
		Cli.spend(key2, 10000000)
		Cli.cleanup
		:timer.sleep(delay)
		Cli.buy_block
		:timer.sleep(delay)
		Cli.to_channel(key1, 8000000, p2)
		Cli.to_channel(key1, 12000000)
		:timer.sleep(delay)
		Cli.buy_block
		:timer.sleep(delay)
		Cli.to_channel(Keys.pubkey, 3000000, p1)
		:timer.sleep(delay)
		Cli.to_channel(key2, 3000000, p1)
		:timer.sleep(delay)
		Cli.buy_block
		:timer.sleep(delay)
		Cli.register(p1)
		Cli.register(p1, p2)
		Cli.send_message(key2, "test1234", p1)
		#:timer.sleep(1000)
		Cli.send_message(key2, "test1235", p1)
		#:timer.sleep(1000)
		Cli.send_message(key2, "test1236", p1)
		:timer.sleep(delay)
		Cli.read_message(0, Keys.pubkey, p2) |> inspect |> IO.puts
		Cli.read_message(1, Keys.pubkey, p2) |> inspect |> IO.puts
		Cli.read_message(2, Keys.pubkey, p2) |> inspect |> IO.puts
		end
	def da do
		#rg
		Cli.delete_account(peer)
	end
	def dm do
		#sm
		p = peer
		a = Cli.inbox_size(p)
		IO.puts("dm size #{inspect a}")
		Cli.delete_message(0, p)
		a = Cli.inbox_size(p)
		IO.puts("dm size #{inspect a}")
	end
	def is do
		#rg
		Cli.inbox_size(peer)
	end
end
