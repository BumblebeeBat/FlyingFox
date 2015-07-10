defmodule Listener do
  use GenServer
	@name __MODULE__
  def init(:ok) do {:ok, []} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def export(l) do    GenServer.call(@name, {hd(l), self(), tl(l)}) end
  def handle_call({type, s, args}, _from, _) do
    spawn(fn() ->     GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
  def main(type, args) do
    case type do
      "add_blocks" -> BlockAbsorber.absorb(hd(args))
      "pushtx" ->
				args
				|> hd
				|> PackWrap.unpack
				|> Mempool.add_tx
				|> PackWrap.pack
      "txs" -> Mempool.txs
      "height" -> KV.get("height")
      "block" -> Blockchain.get_block(hd(args))
      "blocks" -> blocks(String.to_integer(hd(args)), String.to_integer(hd(tl(args))))
      "add_peer" -> Peers.add_peer(hd(args))
      "all_peers" -> Enum.map(Peers.get_all, fn({x, y}) -> y end)
      "status" ->
          h = KV.get("height")
          block = Blockchain.get_block(h)
          if block.data==nil do block = %{data: 1} end
          %Status{height: h, hash: DetHash.doit(block.data), pubkey: Keys.pubkey}
			"register" -> MailBox.register(hd(args))
			"delete_account" -> if CryptoSign.verify_tx(hd(args)) do MailBox.delete_account(hd(args)) else "bad sig" end

			"send_message" -> MailBox.send(hd(args))
			"cost" -> MailBox.cost
			"delete" -> if CryptoSign.verify_tx(hd(args)) do MailBox.delete(hd(args)) else "bad sig" end
			"read_message" ->
				tx = hd(args)
				if CryptoSign.verify_tx(tx) do MailBox.read(tx.data.pub, tx.data.index) else "bad sig" end
			"inbox_size" ->
				tx = hd(args)
				if CryptoSign.verify_tx(tx) do MailBox.size(tx.data.pub) else "bad sig"	end
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
