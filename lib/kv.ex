defmodule KV do
  use GenServer
	@name __MODULE__
  def init(:ok) do {:ok, HashDict.new} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def put(k, v) do    GenServer.cast(@name, {:put, k, v}) end
  def get(k) do       GenServer.call(@name, {:get, k}) end
  def keys do         GenServer.call(@name, :keys) end
  def handle_call(:keys, _from, d) do     {:reply, Dict.keys(d), d} end
  def handle_call({:get, v}, _from, d) do {:reply, d[v], d} end
  def handle_cast({:put, k, v}, d) do     {:noreply, Dict.put(d, k, v)} end
end
