defmodule BlockAbsorber do#this doesn't actually have memory. so I used "x" to fill in spaces.
  use GenServer
  def key do :absorber end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do 
    Blocktree.genesis_state
    Keys.master  
    {:ok, []} 
  end
  def handle_call({:blocks, blocks}, _from, x) do 
    Blocktree.add_blocks(blocks)
    {:reply, :ok, x}
  end
  def absorb(blocks) do GenServer.call(key, {:blocks, blocks}) end
  def buy_block do GenServer.call(key, {:blocks, [Blockchain.buy_block]}) end
  def buy_blocks(n) do Enum.map(1..n, fn(_) -> 
        :timer.sleep(1000)
        buy_block 
        TxCreator.sign
        TxCreator.reveal
      end) 
  end
end
