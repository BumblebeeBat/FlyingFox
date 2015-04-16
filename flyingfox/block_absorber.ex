defmodule BlockAbsorber do#this doesn't actually have memory. so I used "x" to fill in spaces.
  use GenServer
  def key do :absorber end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, []} end
  def handle_call({:blocks, blocks}, _from, x) do 
    Enum.map(blocks, fn(b) -> Blockchain.add_block(b) end)
    {:reply, :ok, x}
  end
  def absorb(blocks) do GenServer.call(key, {:blocks, blocks}) end
  def buy_block do GenServer.call(key, {:blocks, [BlockchainPure.buy_block]}) end
  def buy_blocks(n) do Enum.map(1..n, fn(_) -> 
        :timer.sleep(1000)
        buy_block 
        TxCreator.sign
        TxCreator.reveal
      end) 
  end
  def test do
    import Supervisor.Spec
    children = [ worker(KV, []),
                 worker(Keys, []),
                 worker(Mempool, []),
                 worker(BlockAbsorber, [])]
    {:ok, pid}=Supervisor.start_link(children, strategy: :one_for_one)
    Blockchain.genesis_state
    Keys.master
    TxCreator.sign
    absorb([BlockchainPure.buy_block])
  end  
end
