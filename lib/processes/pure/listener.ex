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
      "pushtx" -> fee_filter(hd(args))
      "txs" -> Mempool.txs
      "height" -> KV.get("height")
      "block" -> Blockchain.get_block(hd(args))
      "blocks" -> blocks(hd(args), hd(tl(args)))
      "add_peer" -> Peers.add_peer(hd(args))
      "all_peers" -> Peers.get_all
      "status" ->
          h = KV.get("height")
          block = Blockchain.get_block(h)
          if block.data==nil do block = %{data: 1} end
          [height: h, hash: DetHash.doit(block.data)]
      x -> IO.puts("listener is not a command #{inspect x}")
    end
  end
  def fee_filter(tx) do
    cond do
      tx.data.fee<10000 -> "low-fee tx are blocked on this node"
      true -> Mempool.add_tx(tx)
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
