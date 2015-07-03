defmodule Cli do
  defp lh do "localhost" end
  defp lp do Port.port end
	defp me do %Peer{port: lp, ip: lh} end
	def talk(msg, peer) do
		#IO.puts("cli #{inspect peer}")
		cond do
			is_list(peer) ->
				Tcp.get(peer[:ip], peer[:port], msg)
			true ->
				Tcp.get(peer.ip, peer.port, msg)
		end
	end
	def local_talk(msg) do Tcp.get_local(me.ip, me.port+1000, msg) end
  def add_block(block, peer \\ me) do talk([:add_block, block], peer) end
  def txs(peer \\ me) do talk([:txs], peer) end
  def pushtx(tx, peer \\ me) do	talk([:pushtx, tx], peer) end
  def fast_blocks(start, finish, peer \\ me) do
		#only use 1 network message. might not grab all blocks
		talk(["blocks", start, finish], peer)
	end
	def blocks(n, i, peer \\ me, out \\ []) do
		#n is how many to download,
		#i is the index to start downloading from.
		#grabs all blocks in the range, even if it needs multiple network messages.
    lo = length(out)
    cond do
      lo >= n -> out
      true -> blocks(n - 1, i, peer, out ++ fast_blocks(i+lo, i+n, peer))
    end		
	end
  def add_peer(peer, pr \\ me) do
		talk(["add_peer", peer], pr) end
  def all_peers(peer \\ me) do
		talk(["all_peers"], peer) end
  def status(peer \\ me) do
		talk(["status"], peer) end
  def buy_block do
		out = local_talk([:buy_block])
		cleanup
		out
	end
	def cleanup do
		local_talk([:cleanup])
	end
	def buy_blocks_helper(n) do
		1..n |> Enum.map(fn(_) ->
			local_talk([:buy_block])
			cleanup
			:timer.sleep(1000)
		end)
	end
  def buy_blocks(n) do spawn(fn -> buy_blocks_helper(n) end) end
  def spend(amount, to) do
		if is_binary(amount) do
			amount = String.to_integer(amount)
		end
		local_talk([:spend, amount, to])
	end
  def stop(peer \\ me) do local_talk([:stop])	end
	def close_channel_fast(pub, amount, amount2) do local_talk([:close_channel_fast, pub, amount, amount2])	end
	def close_channel_timeout(key) do local_talk([:close_channel_fast, key]) end
	def channel_spend(key, amount) do local_talk([:channel_spend, key, amount]) end
	def sign(o) do local_talk([:sign, o])	end
	def register(peer \\ me) do
		#tx = ChannelManager.send(peer, Constants.default_channel_balance)
		talk(["register", tx], peer)
	end
	def delete_account(peer \\ me) do
		%DeleteAccount{}
		talk([], peer)
	end
	def send_message(peer \\ me) do
		%Message{}
		talk([], peer)
	end
	def cost(peer \\ me) do talk(["cost"], peer) end
	def delete(peer \\ me) do
		%DeleteMessage{index: 0, pub: ""}
		talk([], peer)
	end
	def read_message(peer \\ me) do#something with a pub and an index...
		talk([], peer)
	end
	def inbox_size(peer \\ me) do
		talk([], peer)
	end
end
