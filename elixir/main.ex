defmodule Main do
  def start do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    Peers.start
    Listener.start
    Tcp.start(Listener.port, &(Listener.talk(&1)))
  end
end
