defmodule Port do
	use GenServer
	@name __MODULE__
	def init(x) do {:ok, x} end
	def start_link(a, b) do GenServer.start_link(__MODULE__, [internal: a, external: b, tcp: 8000, tcp_internal: 8001], [name: @name]) end
	def port(id) do GenServer.call(@name, {:port, id}) end
	def handle_call({:port, id}, _from, d) do
		{:reply, d[id], d}
	end
	def internal do port(:internal) end
	def external do port(:external) end
	def round_robin do
		:random.seed(:erlang.now)
		trunc(Constants.max_nodes*:random.uniform)
	end
	def next(key) do GenServer.cast(@name, {:next, key}) end
	def handle_cast({:next, key}, d) do
		true = ((key == :internal) or (key == :external))
		x=0
		if key == :internal do
			x=1000
		end
		{:noreply, Dict.put(d, key, round_robin+Constants.tcp_port+x)}
		#{:noreply, {elem(d, 0), round_robin+Constants.tcp_port}}
		#{:noreply, {round_robin+Constants.tcp_port, round_robin+Constants.tcp_port+Constants.port_d}}
		#they both need to change somehow?
	end	
end
