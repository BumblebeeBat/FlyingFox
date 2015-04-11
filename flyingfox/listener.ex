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
    #IO.puts("blocks #{inspect out}")
    cond do
      byte_size(PackWrap.pack(out)) > max -> tl(out)
      start < 0 -> blocks(1, finish, out)
      start > finish -> out
      true -> blocks_helper(start+1, finish, [KV.get(to_string(start))|out])
    end
  end
  def fee_filter(tx) do
    cond do
      tx[:data][:fee]<10000 -> "low-fee tx are blocked on this node"
      true -> Mempool.add_tx(tx)
    end
  end
  def looper do
    receive do    
      ["add_block", block, s] -> 
        out = BlockAbsorber.absorb(block)
        s=s
      ["pushtx", tx, s] -> 
        out = fee_filter(tx)
        s=s
      ["txs", s] -> 
        out = Mempool.txs
        s=s
      ["height", s] -> 
        out = KV.get("height")
        s=s
      ["block", n, s] -> 
        out = KV.get(to_string(n))
        s=s
      ["blocks", start, finish, s] -> 
        out = flip(blocks(start, finish))#KV.get(to_string(n))
        s=s
      ["add_peer", peer, s] -> 
        #don't re-add peers that are already in the system. Attacker could delete all our peers that way.
        out = Peers.add_peer(peer)
        s=s
      ["all_peers", s] -> 
        out = Peers.get_all
        s=s
      ["status", s] -> 
        h = KV.get("height")
        if (h<1) do
          out = [height: 0, hash: ""]
        else
          block = KV.get(to_string(h))
          out = [height: h, 
                 hash: block[:data][:hash]]
        end
        s=s
      x ->
        IO.puts("listener error: #{inspect x}")
    end
    send s, ["ok", out]
    looper
  end
  def key do :listen end
  def port do 6664 end
  def start do#instead of starting once, have a seperate thread for each connection.
    {:ok, pid}=Task.start_link(fn -> looper end)
    Process.register(pid, key)
    :ok    
  end
  def talk(k) do
    k=k++[self()]
    send(key, k)
    receive do
      ["ok", s] -> s
    end
  end
  def add_block(block) do
    talk(["add_block", block])
  end
  def test do
    Main.start
    add_block(BlockchainPure.buy_block)
  end
end

