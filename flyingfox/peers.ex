defmodule Peers do#this module is a database of who your peers are, and other data useful for networking that isn't under consensus.
  def peer_key(peer) do to_string(peer[:port]) <> "$" <> peer[:host] end
  def put(mem, peer) do Dict.put(mem, peer_key(peer), peer) end
  def looper(mem) do
    receive do    
      ["get_all", s] -> send s, [:ok, mem]
      ["add_peer", peer, s] -> 
        send s, [:ok, :ok]
        mem=put(mem, peer)
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
    send(key, k)
    receive do
      [:ok, s] -> s
    end
  end
  def add_peer(peer) do talk(["add_peer", peer, self()]) end
  def get_all do talk(["get_all", self()]) end
  def test do
    start
    add_peer([port: 8088, host: "www.google.com"])
  end
end
