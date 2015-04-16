defmodule Listener do
  #the purpose of this module is to accept connections from multiple peers, and to be able to respond to their messages in parallel
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
  def fee_filter(tx) do
    cond do
      tx[:data][:fee]<10000 -> "low-fee tx are blocked on this node"
      true -> Mempool.add_tx(tx)
    end
  end
  def main(type, args) do
    case type do
      "add_block" -> BlockAbsorber.absorb(hd(args))
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
      send s, ["ok", f.()]
    end)
  end
  def looper do
    receive do
      [type, s, args] -> spawn_send(s, (fn() -> main(type, args) end))
      x -> IO.puts("listener error: #{inspect x}")
    end
    looper
  end
  def talk(type, args) do
    send(key, [type, self(), args])
    receive do
      ["ok", s] -> s
    end
  end
  def export(x) do talk(hd(x), tl(x)) end
  def key do :listen end
  def port do 6664 end
  def start do#instead of starting once, have a seperate thread for each connection.
    pid=spawn_link(fn -> looper end)
    Process.register(pid, key)
    pid
  end
  def add_block(block) do
    talk("add_block", [block])
    #talk(["add_block", block])
  end
  def test do
    Main.start
    add_block(BlockchainPure.buy_block)
  end
end

