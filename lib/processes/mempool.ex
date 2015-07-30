defmodule Mempool do
	#maybe this should be split into 2 threads. the lower one stores the mempool, and the upper one pre-checks txs to see if they are worth giving to the mempool.
	#I am worried it is too easy for an attacker to empty the mempool by feeding in txs that make it crash.
  use GenServer
  @name __MODULE__
  def init(:ok) do {:ok, []} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def dump do         GenServer.cast(@name, :dump) end
  def add_tx(tx) do GenServer.cast(@name, {:add_tx, tx}) end
  def txs do          GenServer.call(@name, :txs) end
  def handle_cast(:dump, _x) do       {:noreply, []} end
  def handle_call(:txs, _from, x) do {:reply, x, x} end
  def handle_cast({:add_tx, tx}, x) do
		h = KV.get("height")
		prev_hash = nil
		if h > 0 do prev_hash = Blockchain.blockhash(Blockchain.get_block(h)) end
		IO.puts("possibly adding tx #{inspect tx}")
		cond do
			not is_map(tx) -> "not map"
			tx.data.__struct__ == :Elixir.Send and tx.data.fee < Constants.min_tx_fee_this_node -> "low-fee tx are blocked on this node"
			not VerifyTx.check_tx(tx, x, prev_hash) -> "invalid tx"
			true ->
				IO.puts("adding tx #{inspect tx}")
				x = [tx|x]
		end
		{:noreply, x}
  end
end
