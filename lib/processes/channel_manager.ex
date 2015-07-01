defmodule ChannelManager do
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do   GenServer.start_link(__MODULE__, %HashDict{}, name: @name) end
	def update(pub, c) do GenServer.cast(@name, {:update, pub, c}) end
	def get(pub) do       GenServer.call(@name, {:get, pub}) end
  def get_all do        GenServer.call(@name, :get_all) end

  def handle_call(:get_all, _from, mem) do {:reply, mem, mem} end
  def handle_call({:get, pub}, _from, mem) do
		out = mem[pub]
		if out == nil do
			out = %ChannelBlock{}
		end
		{:reply, out, mem} end
  def handle_cast({:update, pub, channel},  mem) do
		{:noreply, HashDict.put(mem, pub, channel)}
	end
	def del(pub) do update(pub, nil) end	
end
