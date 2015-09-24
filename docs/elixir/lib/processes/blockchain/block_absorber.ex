defmodule BlockAbsorber do
  use GenServer
  @name __MODULE__
  def init(_args) do 
    Blocktree.genesis_state
    {:ok, []} 
  end
  def start_link do     GenServer.start_link(__MODULE__, :ok, name: @name) end
  def absorb(blocks) do GenServer.cast(@name, {:blocks, blocks}) end
  def buy_block do      GenServer.cast(@name, {:blocks, [Blockchain.buy_block]}) end
  def handle_cast({:blocks, blocks}, []) do
		Blocktree.add_blocks(blocks)
		{:noreply, []}
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
