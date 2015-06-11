defmodule BlockAbsorber do
  use GenServer
  @name __MODULE__
  def init(_args) do 
    Blocktree.genesis_state
    Keys.master  
    {:ok, []} 
  end
  def start_link do     GenServer.start_link(__MODULE__, :ok, name: @name) end
  def absorb(blocks) do GenServer.call(@name, {:blocks, blocks}) end
  def buy_block do      GenServer.call(@name, {:blocks, [Blockchain.buy_block]}) end
  def handle_call({:blocks, blocks}, _from, state) do 
    Blocktree.add_blocks(blocks)
    {:reply, :ok, state}
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
