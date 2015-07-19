defmodule Inbox do 
	#Record all the sent messages.
	#Keep a hashdict sorted by partner's pubkey. Tag each message so we know if it was sent or recieved. Order messages by timestamp.
  @name __MODULE__
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(_) do {:ok, %HashDict{}} end
	def nth(index, l) do
		cond do
			l == nil -> nil
			l == [] -> nil
			index == 0 -> hd(l)
			true -> nth(index - 1, tl(l))
		end
	end
	def handle_call({:peers}, _from, db) do {:reply, HashDict.keys(db), db} end
	def handle_call({:size, pub}, _from, db) do
		out = 0
		if db[pub] != nil do out = length(db[pub]) end
		{:reply, out, db} end
	def handle_call({:read, pub, index}, _from, db) do {:reply, nth(index, db[pub]), db} end
	def handle_cast({:rec, msg}, db) do
		other = [msg.from, msg.to] |> Enum.filter(&(&1 != Keys.pubkey)) |> hd
		if other == [] do
			IO.puts("can't send a message to yourself")
			{:noreply, db}
		else
			a = db[other]
			if a == nil do a = [] end
			{:noreply, HashDict.put(db, other, a ++ [msg])}
		end
	end
	def handle_cast({:del, pub, index}, db) do {:noreply, HashDict.put(db, pub, List.delete_at(db[pub], index))} end
	def peers do GenServer.call(@name, {:peers}) end
	def size(pub) do GenServer.call(@name, {:size, pub}) end
	def read_message(pub, index) do GenServer.call(@name, {:read, pub, index}) end
	def record_message(msg) do GenServer.cast(@name, {:rec, msg}) end
	def delete_message(pub, index) do GenServer.cast(@name, {:del, pub, index}) end
end

