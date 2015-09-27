defmodule Port do
	use GenServer
	@name __MODULE__
	def init(x) do {:ok, x} end
	def start_link(a) do GenServer.start_link(__MODULE__, a, [name: @name]) end
	def port do GenServer.call(@name, :port) end
	def handle_call(:port, _from, d) do
		{:reply, d, d}
	end
	def round_robin do
		:random.seed(:erlang.now)
		trunc(Constants.max_nodes*:random.uniform)
	end
	def next(key) do GenServer.cast(@name, {:next, key}) end
	def handle_cast({:next, key}, d) do {:noreply, round_robin+Constants.tcp_port} end	
end
