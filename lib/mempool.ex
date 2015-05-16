defmodule Mempool do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: @name])
  end
  def dump do
    GenServer.cast(@name, :dump)
  end
  def add_tx(tx) do
    GenServer.cast(@name, {:add_tx, tx})
  end
  def txs do
    GenServer.call(@name, :txs)
  end
  def init(:ok) do
    {:ok, []}
  end
  def handle_cast({:add_tx, tx}, x) do 
    h = KV.get("height")
    if h < 1 do prev_hash = nil
    else
      prev_hash = Blocktree.blockhash(Blockchain.get_block(h))
    end
    if VerifyTx.check_tx(tx, x, prev_hash) do x=[tx|x] end
    {:noreply, x}
  end
  def handle_cast(:dump, x) do
    {:noreply, []}
  end
  def handle_call(:txs, _from, x) do
    {:reply, x, x}
  end
end
