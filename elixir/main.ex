defmodule Main do
  def start do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    Tcp.start(BlockAbsorber.port, &(BlockAbsorber.absorb(&1)))
    Tcp.start(Mempool.port, &(Mempool.talk(&1)))
  end
  def test do
    start
    #abcd=Api.buy_block
    #IO.puts("abcd: #{inspect abcd}")
    IO.puts inspect Api.txs
  end
end
