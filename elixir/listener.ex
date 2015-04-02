defmodule Listener do
  #the purpose of this module is to accept connections from multiple peers, and to be able to respond to their messages in parallel
  def looper(mem) do
    out = receive do    
      ["add_peer", peer, s] -> 
        Peers.add_peer(peer)
        s=s
      ["add_block", block, s] -> 
        BlockAbsorber.absorb(block)
        s=s
      ["pushtx", tx, s] -> 
        Mempool.add_tx(tx)
        s=s
      ["txs", s] -> 
        Mempool.txs
        s=s
      ["height", s] -> 
        KV.get("height")
        s=s
      ["block", n, s] -> 
        KV.get(to_string(n))
        s=s
    end
    send s, ["ok", out]
    looper mem
  end
  def key do :listen end
  def port do 6664 end
  def start do#instead of starting once, have a seperate thread for each connection.
    {:ok, pid}=Task.start_link(fn -> looper(%HashDict{}) end)
    Process.register(pid, key)
    :ok    
  end
  def talk(k) do
    send(key, k)
    receive do
      [:ok, s] -> s
    end
  end

end

