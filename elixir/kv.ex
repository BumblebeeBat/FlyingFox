defmodule KV do
  defp loop(map) do
    receive do
      [keys, caller] ->
        send caller, [:ok, Dict.keys(map)]
        loop(map)
      ["get", key, caller] ->
        a = Dict.get(map, key)
        if a == nil do a = Constants.empty_account end
        send caller, [:ok, a]
        loop(map)
      [:put, key, value] ->
        loop(Dict.put(map, key, value))
    end
  end
  def key do :kv end
  def port do 6668 end
  def start do
    {:ok, pid}=Task.start_link(fn -> loop(%HashDict{}) end)
    Process.register(pid, key)
    :ok
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


