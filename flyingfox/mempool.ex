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
end
