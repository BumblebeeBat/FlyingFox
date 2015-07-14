defmodule Cli do
  defp lh do "localhost" end
  defp lp do Port.port end
	defp me do %Peer{port: lp, ip: lh} end
	def talk(msg, peer) do
		cond do
			is_list(peer) ->
				Tcp.get(peer[:ip], peer[:port], msg)
			true ->
				Tcp.get(peer.ip, peer.port, msg)
		end
	end
	def local_talk(msg, peer \\ me) do Tcp.get_local(peer.ip, peer.port+1000, msg) end
	def packer(o, f) do o |> PackWrap.pack |> f.() |> PackWrap.unpack end
  def add_blocks(blocks, peer \\ me) do
		#IO.puts("add blocks #{inspect blocks}")
		blocks |> packer(&(talk([:add_blocks, &1], peer))) end
  def txs(peer \\ me) do talk([:txs], peer) end
  def pushtx(tx, peer \\ me) do	tx |> packer(&(talk([:pushtx, &1], peer))) end
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
  def add_peer(peer, pr \\ me) do peer |> packer(&(talk(["add_peer", &1], pr)))	end
  def all_peers(peer \\ me) do
		talk(["all_peers"], peer) end
  def status(peer \\ me) do
		talk(["status"], peer) end
  def buy_block(peer \\ me) do
		out = local_talk([:buy_block], peer)
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
  def spend(to, amount) do
		if is_binary(amount) do
			amount = String.to_integer(amount)
		end
		local_talk([:spend, to, amount])
	end
	def to_channel(to, amount) do
		if is_binary(amount) do
			amount = String.to_integer(amount)
		end
		local_talk([:to_channel, to, amount])
	end
	def close_channel_fast(pub) do local_talk([:close_channel_fast, pub])	end
	def close_channel_slasher(tx) do tx |> packer(&(local_talk([:close_channel_slasher, &1]))) end
	def close_channel_timeout(key) do local_talk([:close_channel_timeout, key]) end
	def channel_spend(key, amount, peer \\ me) do local_talk([:channel_spend, key, amount], peer) end
	def channel_accept(tx, amount, peer \\ me) do tx |> packer(&(talk([:accept, &1, amount],peer))) end
	def channel_state(key) do local_talk([:channel_state, key]) end
	def new_key(p \\ me) do local_talk([:newkey], p) end
	def load_key(pub, priv) do local_talk([:loadkey, pub, priv]) end
	def sign(o, p \\ me) do o |> packer(&(local_talk([:sign, &1], p))) end
	def register(peer \\ me) do
		#we may need to do a to_channel first?
		IO.puts("cli register")
		pub = Cli.status(peer).pubkey
		tx = ChannelManager.spend(pub, Constants.registration)
		IO.puts("cli register tx #{inspect tx}")
		%{payment: tx, pub: Keys.pubkey}
		|> packer(&(talk(["register", &1], peer)))
	end
	def delete_account(peer \\ me) do
		%DeleteAccount{pub: Keys.pubkey}#this needs to be a struct.
		|> Keys.sign
		|> packer(&(talk(["delete_account", &1], peer)))
	end
	def send_message(pub, msg, peer \\ me) do
		tx = ChannelManager.spend(pub, round(cost(peer)*1.05))
		%SendMessage{payment: tx, to: pub, msg: msg, pub: Keys.pubkey}
		|> Keys.sign
		|> packer(&(talk(["send_message", &1], peer)))
	end
	def cost(peer \\ me) do talk(["cost"], peer) end
	def delete_message(index, peer \\ me) do
		%DeleteMessage{pub: Keys.pubkey, index: index}
		|> Keys.sign
		|> packer(&(talk(["delete", &1], peer)))
		#%DeleteMessage{index: 0, pub: ""}
	end
	def read_message(index, peer \\ me) do
		%ReadMessage{pub: Keys.pubkey, index: index}
		|> Keys.sign
		|> packer(&(talk(["read_message", &1], peer)))
	end
	def inbox_size(peer \\ me) do
		%InboxSize{pub: Keys.pubkey}
		|> Keys.sign
		|> packer(&(talk(["inbox_size", &1], peer)))
	end
end
