defmodule KV do
  def start do
    {:ok, pid}=Task.start_link(fn -> loop(%HashDict{}) end)
		Process.register(pid, :kv)
		a=Accounts.empty
		a = Dict.put(a, :amount, 2_100_000_000_000_000)
		{creator_pub, creator_priv} = {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="}
		KV.put(creator_pub, a)
		:ok
  end
	def get(k) do
		send(:kv, {:get, k, self()})
		receive do
			{:ok, s} -> s
		end
	end
	def keys do
		send(:kv, {:keys, self()})
		receive do
			{:ok, s} -> s
		end
	end
	def put(k, v) do
		send(:kv, {:put, k, v})
	end
  defp loop(map) do
    receive do
			{:keys, caller } ->
				send caller, {:ok, Dict.keys(map)}
				loop(map)
      {:get, key, caller} ->
        send caller, {:ok, Dict.get(map, key)}
        loop(map)
      {:put, key, value} ->
        loop(Dict.put(map, key, value))
    end
  end
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


