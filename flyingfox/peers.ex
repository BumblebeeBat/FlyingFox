defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  def peer_key(peer) do to_string(peer[:port]) <>"$"<> peer[:ip] end
  def looper(mem) do
    receive do    
      ["get_all", s] -> send s, [:ok, mem]
      ["get", peer,s] -> send s, [:ok, Dict.get(mem, peer_key(peer))]
      ["update", peer, s] ->
        send s, [:ok, :ok]
        mem=Dict.put(mem, peer_key(peer), peer)
    end
    looper mem
  end
  def key do :peers end
  def start do
    {:ok, pid}=Task.start_link(fn -> looper(%HashDict{}) end)
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
    {a, b, c} = :os.timestamp
    b*1000+div(c, 1000)
  end
  def new_peer(peer) do 
    peer |> Dict.put(:time, timestamp) |> Dict.put(:height, 0) |> Dict.put(:hash, "") end
  def update(peer) do talk(["update", new_peer(peer)]) end
  def add_peer(peer) do 
    if not(peer_key(peer) in Dict.keys(get_all)) do
      update(peer)
    end 
  end
  def get(peer) do talk(["get", peer]) end
  def get_all do talk(["get_all"]) end
  def test do
    start
    add_peer([port: 8088, ip: "www.google.com"])
  end
end
