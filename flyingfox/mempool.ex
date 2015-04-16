defmodule Mempool do
  use GenServer
  def key do :txs end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, []} end
  def handle_cast(:dump, x) do {:noreply, []} end
  def handle_cast({:add_tx, tx}, x) do 
  if VerifyTx.check_tx(tx, x) do x=[tx|x] end
  {:noreply, x}
  end
  def handle_call(:txs, _from, x) do {:reply, x, x} end
  def dump do GenServer.cast(key, :dump) end
  def add_tx(tx) do GenServer.cast(key, {:add_tx, tx}) end
  def txs do GenServer.call(key, :txs) end
  def test do
    import Supervisor.Spec
    children = [ worker(KV, []),
                 worker(Keys, []),
                 worker(Mempool, []) ]
    {:ok, pid}=Supervisor.start_link(children, strategy: :one_for_one)
    dump
    Blockchain.genesis_state
    txs
    Keys.master
    TxCreator.sign
    txs
  end
end
