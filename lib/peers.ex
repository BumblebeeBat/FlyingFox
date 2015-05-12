defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  use GenServer
  @name __MODULE__
  def start_link() do: GenServer.start_link(__MODULE__, [], name: @name])
  def get_all do: GenServer.call(@name, :get_all)
  def get(peer) do: GenServer.call(@name, {:get, peer})
  def timestamp do 
    {_, b, c} = :os.timestamp
    b * 1000 + div(c, 1000)
  end
  def update(peer, height, hash) do
    p = %Peer{peer | time: timestamp, height: height, hash: hash}
    GenServer.cast(@name, {:update, p})
  end
  def peer_key(peer) do: String.to_atom(to_string(peer.port) <>"$"<> peer.ip)#to atom is VERY DANGEROUS!!!
  def init(args) do: {:ok, args}
  def handle_call(:get_all, _from, state) do {:reply, state, state} end
  def handle_call({:get, peer}, _from, mem) do {:reply, Dict.get(mem, peer_key(peer)), mem} end
  def handle_cast({:update, peer}, mem) do 
    {:noreply, Dict.put(mem, peer_key(peer), peer)}
  end
  def new_peer(peer) do: update(peer, 0, "")
  def add_peer(peer) do 
  cond do
    is_binary(peer) -> false
    is_integer(peer) -> false
    peer_key(peer) in Dict.keys(get_all) -> false
    true -> new_peer(peer)
    end
  end
end
