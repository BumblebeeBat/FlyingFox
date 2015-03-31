defmodule Main do
  def start do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    BlockAbsorber.start
    #Tcp.start(BlockAbsorber.port, &(BlockAbsorber.talk(&1)))
    #Tcp.start(BlockAbsorber.port, &(BlockAbsorber.talk(&1)))
    #Tcp.talk("localhost", BlockAbsorber.port, Blockchain.buy_block)
    #Tcp.talk("localhost", BlockAbsorber.port, Blockchain.buy_block)
  end
  def test do
    start
    port=BlockAbsorber.port
    Tcp.start(port, &(BlockAbsorber.absorb(&1)))
    block=Blockchain.buy_block
    #BlockAbsorber.absorb(block)
    #IO.puts("block #{inspect block}")
    abcd=Tcp.talk("localhost", port, block)
    IO.puts("abcd: #{inspect abcd}")
             
    
  end
end
