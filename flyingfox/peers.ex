defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  def to_tuple_list(dict) do Dict.keys(dict) |> Enum.map(&({&1, dict[&1]})) end
  def peer_key(peer) do to_string(peer[:port]) <>"$"<> peer[:ip] end
  def looper(mem) do
    receive do    
      ["get_all", s] -> send s, [:ok, to_tuple_list(mem)]
      ["get", peer,s] -> send s, [:ok, Dict.get(mem, peer_key(peer))]
      ["update", peer, s] ->
        send s, [:ok, :ok]
        mem=Dict.put(mem, peer_key(peer), peer)
    end
    looper mem
  end
  def key do :peers end
  def start do
    pid=spawn_link(fn -> looper(%HashDict{}) end)
    Process.register(pid, key)
    :ok    
  end
  def talk(k) do
    send(key, k++[self()])
    receive do
      [:ok, s] -> s
    end
  end
  def timestamp do 
    {_, b, c} = :os.timestamp
    b*1000+div(c, 1000)
  end
  def new_peer(peer) do update(peer, 0, "") end
  def update(peer, height, hash) do 
    p = peer |> Dict.put(:time, timestamp) |> Dict.put(:height, height) |> Dict.put(:hash, hash) 
    talk(["update", p]) 
  end
  def add_peer(peer) do 
  cond do
    is_binary(peer) -> false
    is_integer(peer) -> false
    peer_key(peer) in Dict.keys(get_all) -> false
    true -> new_peer(peer)
    end
  end
  def get(peer) do talk(["get", peer]) end
  def get_all do talk(["get_all"]) end
  def test do
    start
    add_peer([port: 8088, ip: "www.google.com"])
  end
end
