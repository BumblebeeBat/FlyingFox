defmodule Listener do
  use GenServer
	@name __MODULE__
  def init(:ok) do {:ok, []} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def export(l) do GenServer.call(@name, {hd(l), self(), tl(l)}) end
  def handle_call({type, s, args}, _from, _) do
    spawn(fn() -> GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
	def packer(o, f) do o |> PackWrap.unpack |> f.() |> PackWrap.pack end
	def sig(o, f) do o |> hd |> packer(&(if CryptoSign.verify_tx(o) do f.(&1.data) else	"bad sig"	end)) end
	def main(type, args) do
    case type do
      "add_blocks" ->
				IO.puts("add_blocks #{inspect args}")
				args |> hd |> packer(&(BlockAbsorber.absorb(&1)))
      "pushtx" -> args |> hd |> packer(&(Mempool.add_tx(&1)))
      "txs" -> Mempool.txs
      "height" -> KV.get("height")
      "block" -> Blockchain.get_block(hd(args))
      "blocks" -> blocks(String.to_integer(hd(args)), String.to_integer(hd(tl(args))))
      "add_peer" -> args |> hd |> packer(&(Peers.add_peer(&1)))
      "all_peers" -> Enum.map(Peers.get_all, fn({x, y}) -> y end)
      "status" ->
          h = KV.get("height")
          block = Blockchain.get_block(h)
          if block.data==nil do block = %{data: 1} end
          %Status{height: h, hash: DetHash.doit(block.data), pubkey: Keys.pubkey}
			"cost" -> MailBox.cost
			"register" -> args |> packer(&(MailBox.register(&1.payment, &1.pub)))
			"delete_account" -> args |> sig(&(MailBox.delete_account(&1.pub)))
			"send_message" ->   args |> sig(&(MailBox.send(&1.payment, &1.to, &1.msg, &1.pub)))
			"delete" ->         args |> sig(&(MailBox.delete(&1.pub, &1.index)))
			"read_message" ->   args |> sig(&(MailBox.read(&1.pub, &1.index)))
			"inbox_size" ->     args |> sig(&(MailBox.size(&1.pub)))
			"accept" -> ChannelManager.accept(hd(args), 1000)
      x -> IO.puts("listener is not a command #{inspect x}")
    end
  end
  def blocks(start, finish, out \\ []) do
    finish |> min(KV.get("height")) |> blocks_helper(start, out)
  end
  def blocks_helper(finish, start, out) do
    block = Blockchain.get_block(start)
    cond do
			byte_size(inspect out) > Constants.message_size/2 -> tl(out)
      start < 0 -> blocks_helper(finish, 1, out)
      start > finish -> out
      block == nil -> blocks_helper(finish, start+1, out)
      true -> blocks_helper(finish, start+1, [block|out])
    end
  end
end
