defmodule MailNodes do
	#need to keep track of a list of nodes where we are registered.
  @name __MODULE__
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(_) do {:ok, []} end
	def handle_cast({:reg, peer}, db) do {:noreply, [peer|db]} end
	def handle_cast({:del, peer}, db) do {:noreply, db |> Enum.filter(&(&1.port != peer.port or &1.ip != peer.ip))}	end
	def handle_call(:all, _from, db) do {:reply, db, db} end
	def register(peer, pub) do
		GenServer.cast(@name, {:reg, peer})
		ChannelManager.spend(pub, Constants.registration)
	end
	def delete_account(peer) do GenServer.cast(@name, {:del, peer}) end
	def all do GenServer.call(@name, :all) end
end
