defmodule Port do
	use GenServer
	@name __MODULE__
	def init([internal, external]) do {:ok, {internal, external}} end
	def start_link(a, b) do GenServer.start_link(__MODULE__, [a, b], [name: @name]) end
	def port do GenServer.call(@name, :port) end
	def internal do elem(port, 0) end
	def external do elem(port, 1) end
	def handle_call(:port, _from, d) do {:reply, d, d} end
	
	def next do GenServer.cast(@name, :next) end
	def handle_cast(:next, d) do {:noreply, {elem(d, 0), elem(d, 1)+1}} end	

	#def reset(new) do GenServer.cast(@name, {:reset, new}) end
	#def handle_cast({:reset, new}, d) do {:noreply, new} end	
end
