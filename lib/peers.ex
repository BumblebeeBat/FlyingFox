defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  use GenServer
  def key do :peers end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, []} end
  def handle_call(:get_all, _from, mem) do {:reply, mem, mem} end
  def handle_call({:get, peer}, _from, mem) do {:reply, Dict.get(mem, peer_key(peer)), mem} end
  def handle_cast({:update, peer}, mem) do 
    b=Dict.put(mem, peer_key(peer), peer)
    {:noreply, b} 
  end
  def peer_key(peer) do String.to_atom(to_string(peer[:port]) <>"$"<> peer[:ip]) end#to atom is VERY DANGEROUS!!!
  def get_all do GenServer.call(key, :get_all) end
  def get(peer) do GenServer.call(key, {:get, peer}) end
  def timestamp do 
    {_, b, c} = :os.timestamp
    b*1000+div(c, 1000)
  end
  def update(peer, height, hash) do
    p = peer |> Dict.put(:time, timestamp) |> Dict.put(:height, height) |> Dict.put(:hash, hash) 
    GenServer.cast(key, {:update, p})
  end
  def new_peer(peer) do update(peer, 0, "") end
  def add_peer(peer) do 
  cond do
    is_binary(peer) -> false
    is_integer(peer) -> false
    peer_key(peer) in Dict.keys(get_all) -> false
    true -> new_peer(peer)
    end
  end
end
