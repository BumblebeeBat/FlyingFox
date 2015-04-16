defmodule Listener do
  use GenServer
  def key do :listen end
  def port do 6666 end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, []} end#
  def handle_cast({type, s, args}, _) do 
    spawn_send(s, (fn() -> main(type, args) end))
    {:noreply, []}
  end
  def export(l) do 
    GenServer.cast(key, {hd(l), self(), tl(l)}) 
    receive do
      [:ok, x] -> x
    end
  end
  #def export(x) do GenServer.call(key, {:api, hd(x), tl(x)}) end
  #def add_block(block) do export([:add_block, block]) end
  #def pushtx(tx) do export([:pushtx, tx]) end
  #def txs do export([:txs]) end
  #def height do export([:height]) end
  #def block(n) do export([:block, n]) end
  #def add_peer(peer) do export([:add_peer, peer]) end
  #def all_peers do export([:all_peers]) end
  #def status do export([:status])
  def main(type, args) do
    case type do
      "add_blocks" -> BlockAbsorber.absorb(hd(args))
      "pushtx" -> fee_filter(hd(args))
      "txs" -> Mempool.txs
      "height" -> KV.get("height")
      "block" -> BlockchainPure.get_block(hd(args))
      "blocks" -> flip(blocks(hd(args), hd(tl(args))))
      "add_peer" -> Peers.add_peer(hd(args))
      "all_peers" -> Peers.get_all
      "status" ->
          h = KV.get("height")
          block = BlockchainPure.get_block(h)
          if block[:data]==nil do block = [data: 1] end
          [height: h, hash: DetHash.doit(block[:data])]
      x -> IO.puts("is not a command #{inspect x}")
    end
  end
  def spawn_send(s, f) do
    spawn_link(fn() ->
      send s, [:ok, f.()]
    end)
  end
  def fee_filter(tx) do
    cond do
      tx[:data][:fee]<10000 -> "low-fee tx are blocked on this node"
      true -> Mempool.add_tx(tx)
    end
  end
  def flip(x, y \\ []) do 
    cond do
      x==[] -> y
      true -> flip(tl(x), [hd(x)|y])
    end
  end
  def max do 10000 end
  def blocks(start, finish, out \\ []) do
    finish = min(finish, KV.get("height"))
    blocks_helper(start, finish, out)
  end
  def blocks_helper(start, finish, out) do
    block = BlockchainPure.get_block(start)
    cond do
      byte_size(PackWrap.pack(out)) > max -> tl(out)
      start < 0 -> blocks(1, finish, out)
      start > finish -> out
      block == Constants.empty_account -> blocks_helper(start+1, finish, out)
      true -> blocks_helper(start+1, finish, [block|out])
    end
  end
  def test do
    import Supervisor.Spec
    children = [ worker(KV, []),
                 worker(Keys, []),
                 worker(Mempool, []), 
                 worker(BlockAbsorber, []),                 
                 worker(Listener, []) ]
    {:ok, pid}=Supervisor.start_link(children, strategy: :one_for_one)
    Blockchain.genesis_state
    Keys.master
    TxCreator.sign
    b=BlockchainPure.buy_block
    export(["add_blocks", [b]])
  end
end
