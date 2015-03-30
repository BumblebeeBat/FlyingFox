defmodule Main do
  def main do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    Tcp.start(BlockAbsorber.port, &(BlockAbsorber.talk(&1)))
  end
end
