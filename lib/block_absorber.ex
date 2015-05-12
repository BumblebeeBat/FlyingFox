defmodule BlockAbsorber do#this doesn't actually have memory. so I used "x" to fill in spaces.
  use GenServer
  @name __MODULE__
  #####
  # API

  def start_link do 
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end
  def absorb(blocks) do
    GenServer.call(@name, {:blocks, blocks})
  end

  def key do :absorber end
  def init(_args) do 
    Blocktree.genesis_state
    Keys.master  
    {:ok, []} 
  end
  def handle_call({:blocks, blocks}, _from, state) do 
    Blocktree.add_blocks(blocks)
    {:reply, :ok, state}
  end
  #def absorb(blocks) do GenServer.call(key, {:blocks, blocks}) end
  def buy_block do 
    GenServer.call(@name, {:blocks, [Blockchain.buy_block]}) 
  end
  def buy_blocks(n) do 
    Enum.map(1..n, fn(_) -> 
        :timer.sleep(1000)
        buy_block 
        TxCreator.sign
        TxCreator.reveal
      end) 
  end
end
