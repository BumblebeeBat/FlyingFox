defmodule Listener do
  #the purpose of this module is to accept connections from multiple peers, and to be able to respond to their messages in parallel
  def looper(mem) do
    receive do    
      ["add_peer", peer, s] -> 
        out = Peers.add_peer(peer)
        s=s
      ["add_block", block, s] -> 
        out = BlockAbsorber.absorb(block)
        s=s
      ["pushtx", tx, s] -> 
        out = Mempool.add_tx(tx)
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
      x ->
        IO.puts("listener error: #{inspect x}")
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
    k=k++[self()]
    send(key, k)
    receive do
      ["ok", s] -> s
    end
  end
  def add_block(block) do
    talk(["add_block", block])
  end
  def test do#test is failing!!!
    Main.start
    add_block(BlockchainPure.buy_block)
  end
end

