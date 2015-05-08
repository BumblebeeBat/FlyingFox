defmodule KV do
  use GenServer
  def key do :kv end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, HashDict.new} end
  def handle_call(:keys, _from, dict) do {:reply, Dict.keys(dict), dict} end
  def handle_call({:get, v}, _from, dict) do
    out = dict[v]
    {:reply, out, dict}
  end
  def handle_cast({:put, k, v}, dict) do {:noreply, Dict.put(dict, k, v)} end
  def put(k, v) do GenServer.cast(key, {:put, k, v}) end
  def get(k) do GenServer.call(key, {:get, k}) end
  def keys do GenServer.call(key, :keys) end
end
