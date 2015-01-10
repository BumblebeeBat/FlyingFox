defmodule Txs do
	def start do
		{:ok, pid}=Task.start_link(fn -> looper([]) end)
		Process.register(pid, :txs)
		:ok
	end
	def looper(mem) do
		receive do
			{:dump} ->
				looper([])
			{:add_tx, tx, s} ->
				send(s, :ok)
				looper([tx|mem])
			{:txs, s} ->
				send(s, {:ok, mem})
				looper(mem)
		end
	end
	def txs() do
		send(:txs, {:txs, self()})
		receive do
			{:ok, mem} -> mem
		end
	end
	def add_tx(tx) do
		send(:txs, {:add_tx, tx, self()})
	end
	def dump() do
		send(:txs, {:dump})
	end
	def test do
		start
		add_tx("55")
		IO.puts inspect txs
		add_tx("550000")
		IO.puts inspect txs
		dump
		IO.puts inspect txs		
	end
end
