defmodule InternalListener do
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def export(l) do GenServer.call(@name, {hd(l), tl(l)}) end
  def handle_call({type, args}, _from, _) do 
    Task.start(fn() -> GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
	def pack(o, f) do
		o |> PackWrap.unpack |> f.() |> PackWrap.pack
	end
  def main(type, args) do
    case type do
      "buy_block" -> BlockAbsorber.buy_block
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "close_channel_fast" -> TxCreator.close_channel_fast(hd(args))
      "close_channel_slasher" ->
				args
				|> hd
				|> pack(&(TxCreator.close_channel_slasher(&1)))
      "close_channel_timeout" -> TxCreator.close_channel_timeout(hd(args))
      "to_channel" -> TxCreator.to_channel(hd(args), hd(tl(args)))
      "channel_spend" -> ChannelManager.spend(hd(args), hd(tl(args)))
      "channel_state" -> ChannelManager.get(hd(args))
      "sign" ->
				args
				|> hd
				|> pack(&(Keys.sign(&1)))
			"cleanup" ->
				TxCreator.sign
				TxCreator.reveal
			"newkey" -> Keys.new
			"loadkey" -> Keys.load(hd(args), hd(tl(args)))
			"register" ->
				#maybe MailNodes.register shoule contain all this logic
				IO.puts("internal register #{inspect args}")
				peer = args |> hd |> PackWrap.unpack
				IO.puts("peer #{inspect peer}")
				pub = Cli.status(peer).pubkey
				%{payment: MailNodes.register(peer, pub), pub: Keys.pubkey}
				|> Cli.packer(&(Cli.talk(["register", &1], peer)))
			"delete_account" ->
				#maybe MailNodes.delete_account shoule contain all this logic
				peer = hd(args)
				MailNodes.delete_account(peer)
				%DeleteAccount{pub: Keys.pubkey}
				|> Keys.sign
				|> Cli.packer(&(Cli.talk(["delete_account", &1], peer)))
				|> ChannelManager.accept(0)
			"send_message" ->
				#maybe a function in inbox shoule contain all this logic
				node = hd(args) |> PackWrap.unpack
				IO.puts("send message node #{inspect node}")
				pub = hd(tl(args))
				IO.puts("send message pub #{inspect pub}")
				IO.puts("args #{inspect args}")
				msg = hd(tl(tl(args))) |> PackWrap.unpack
				node_pub = Cli.status(node).pubkey
				tx = ChannelManager.spend(node_pub, max(round(Cli.cost(node)*1.01), Constants.min_channel_spend))
				Inbox.record_message(%Msg{msg: msg, to: pub, from: Keys.pubkey})
				%SendMessage{payment: tx, to: pub, msg: Encryption.send_msg(msg, pub)}
				|> Cli.packer(&(Cli.talk(["send_message", &1], node)))
			"read_message" ->
				index = hd(args)
				if is_binary(index) do index = String.to_integer(index) end
				pub = hd(tl(args))
				Inbox.read_message(pub, index)
				|> PackWrap.pack
			"inbox_size" -> Inbox.size(hd(args))
			"delete_message" -> Inbox.delete_message(hd(args), hd(tl(args)))
			"inbox_peers" -> Inbox.peers |> PackWrap.pack
			"channel_get" -> args |> hd |> ToChannel.key(Keys.pubkey) |> KV.get |> PackWrap.pack
			"channel_peers" -> HashDict.keys(ChannelManager.get_all)
			"channel_balance" ->
				pub = hd(args)
				on_blockchain = KV.get(ToChannel.key(Keys.pubkey, hd(args)))
				on_channel_manager = ChannelManager.get(pub) |> ChannelManager.top_block
				cond do
					on_blockchain == nil -> 0
					true ->
						if on_blockchain.pub == Keys.pubkey do
							amount = on_blockchain.amount
						else
							amount = on_blockchain.amount2
						end
						if on_channel_manager.data.pub == Keys.pubkey do
							amount = amount + on_channel_manager.data.amount
						else
							amount = amount - on_channel_manager.data.amount
						end
						amount
				end
      x -> IO.puts("is not a command #{inspect x}")
    end
  end
end
