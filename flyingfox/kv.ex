defmodule KV do
  defp looper(map) do
    receive do
      [:keys, caller] ->
        send caller, [:ok, Dict.keys(map)]
        looper(map)
      ["get", key, caller] ->
        a = Dict.get(map, key)
        if a == nil do a = Constants.empty_account end
        send caller, [:ok, a]
        looper(map)
      [:put, key, value] ->
        looper(Dict.put(map, key, value))
    end
  end
  def key do :kv end
  def start do
    pid=spawn_link(fn -> looper(%HashDict{}) end)
    Process.register(pid, key)
    pid
  end
  def talk(k) do
    send(key, k)
    receive do
      [:ok, s] -> s
    end
  end
  def get(k) do talk(["get", k, self()]) end
  def keys do talk([:keys, self()]) end
  def put(k, v) do send(key, [:put, k, v]) end
  def forLoop(f, n) do
    f.(n)
    cond do
      n == 0 -> n
      true   -> forLoop(f, n-1)
    end
  end
  def tester do
    start
    m=1000000
    forLoop(&(KV.put(to_string(&1), to_string(&1*&1))), m)
    #forLoop(&("a"<>KV.get(to_string(&1))), m)
    IO.puts KV.get(to_string(10400))
  end
end


