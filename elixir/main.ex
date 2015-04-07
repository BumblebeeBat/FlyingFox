defmodule Main do
  def start(n \\ 0) do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    Peers.start
    Listener.start
    Tcp.start(n+Listener.port, &(Listener.talk(&1)))
  end
end
