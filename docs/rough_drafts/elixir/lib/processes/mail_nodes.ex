defmodule MailNodes do
	#need to keep track of a list of nodes where we are registered.
  @name __MODULE__
	def db_location do "/mail_nodes" end
  def init(_) do
		x = DB.get_raw(db_location)
		if x == "" do x = [] end
		{:ok, x}
	end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
	def handle_cast({:reg, peer}, db) do
		out = [peer|db]
		DB.put_function(db_location, fn() -> out end)
		{:noreply, out}
	end
	def handle_cast({:del, peer}, db) do
		out = db |> Enum.filter(&(&1.port != peer.port or &1.ip != peer.ip))
		DB.put_function(db_location, fn() -> out end)
		{:noreply, out}
	end
	def handle_call(:all, _from, db) do {:reply, db, db} end
	def register(peer, pub) do
		GenServer.cast(@name, {:reg, peer})
		ChannelManager.spend(pub, Constants.registration)
	end
	def delete_account(peer) do GenServer.cast(@name, {:del, peer}) end
	def all do GenServer.call(@name, :all) end
end
