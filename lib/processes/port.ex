defmodule Port do
	use GenServer
	@name __MODULE__
	def init([internal, external]) do {:ok, {internal, external}} end
	def start_link(a, b) do GenServer.start_link(__MODULE__, [a, b], [name: @name]) end
	def port do GenServer.call(@name, :port) end
	def internal do elem(port, 0) end
	def external do elem(port, 1) end
	def handle_call(:port, _from, d) do {:reply, d, d} end
	def round_robin(current, range) do
		cond do
			current >= range - 1 -> current - range + 2
			true -> current + 1
		end
	end
	def next do GenServer.cast(@name, :next) end
	def handle_cast(:next, d) do
		IO.puts("d #{inspect d}")
		q = Constants.tcp_port
		o = round_robin(elem(d, 1) - q, Constants.max_nodes) + q
		IO.puts("q o #{inspect q} #{inspect o}")
		{:noreply, {elem(d, 0), o}}
	end	
end
