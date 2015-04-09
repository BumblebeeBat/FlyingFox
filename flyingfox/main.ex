defmodule Main do
  def start(n \\ 0) do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    Peers.start
    Listener.start
    p=n+Listener.port
    KV.put("port", p)
    Tcp.start(p, &(Listener.talk(&1)))
    Talker.start
  end
end
