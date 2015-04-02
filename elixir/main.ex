defmodule Main do
  def start do
    KV.start
    IO.puts("1")
    Mempool.start
    IO.puts("1")
    Blockchain.genesis_state
    IO.puts("1")
    BlockAbsorber.start
    IO.puts("2")
    Peers.start
    Listener.start
    Tcp.start(KV.port, &(KV.get(&1)))
    IO.puts("3")
    Tcp.start(Mempool.port, &(Mempool.talk(&1)))
    #Tcp.start(BlockAbsorber.port, &(BlockAbsorber.absorb(&1)))
    Tcp.start(Listener.port, &(Listener.talk(&1)))#6664
    IO.puts("4")
  end
  def test do
    start
    #abcd=Api.buy_block
    #IO.puts("abcd: #{inspect abcd}")
    IO.puts inspect Api.txs
    #Api.pushtx(tx)
  end
end
