defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do GenServer.start_link(__MODULE__, %HashDict{}, name: @name) end
  def update(peer) do GenServer.cast(@name, {:update, %Peer{peer | time: Timer.stamp}}) end
  def get_all do      GenServer.call(@name, :get_all) end
  def get(peer) do    GenServer.call(@name, {:get, peer}) end
  def handle_call(:get_all, _from, state) do {:reply, state, state} end
  def handle_call({:get, peer}, _from, mem) do {:reply, HashDict.get(mem, peer_key(peer)), mem} end
  def handle_cast({:update, peer}, mem) do {:noreply, HashDict.put(mem, peer_key(peer), peer)} end
  def peer_key(peer) do to_string(peer.port) <>"$"<> peer.ip end
  #def new_peer(peer) do update(peer) end
  def add_peer(peer) do 
  cond do
    is_binary(peer) -> false
    is_integer(peer) -> false
    true -> update(peer)
    end
  end
end
