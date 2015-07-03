defmodule Msg do
	defstruct time: 0, msg: "", size: 0, price: 0
end

defmodule MailBox do
  use GenServer
  @name __MODULE__
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(:ok) do {:ok, {%HashDict{}, 0, 0}} end
	def handle_cast({:new, pub}, {db, mailboxes, m}) do
		if	db[pub] == nil do
			{:noreply, {HashDict.put(db, pub, []), mailboxes + 1, m}}
		else
			{:noreply, {db, mailboxes, m}}
		end
	end
	def handle_cast({:del_acc, pub}, {db, mailboxes, messages}) do
		if db[pub] == nil do
			{:noreply, {db, mailboxes, messages}}
		else
			{:noreply, {HashDict.del(db, pub), mailboxes - 1, messages - length(db[pub])}}
		end
	end
	def handle_cast({:send, pub, message, price}, {db, m, messages}) do
		msg = %Msg{msg: message, time: :os.timestamp, size: byte_size(message), price: price}
		dd = HashDict.put(db, pub, [msg|db[pub]])
		{:noreply, {dd, m, messages+1}}
	end
	def handle_cast({:del, pub, index}, {db, m, messages}) do
		dd = HashDict.put(db, pub, List.delete_at(db[pub], index))
		{:noreply, {dd, m, messages - 1}}
	end
	defp nth(index, l) do if index == 0 do hd(l) else nth(index - 1, tl(l)) end end
	def handle_call({:read, pub, index}, _from, {db, b, c}) do {:reply, nth(index, db[pub]), {db, b, c}}	end
	def handle_call({:size, pub}, _from, {db, b, c}) do
		cond do
			db[pub] == nil -> out = 0
			true -> out = length(db[pub])
		end
		{:reply, out, {db, b, c}}
	end
	def handle_call(:status, _from, {db, m, n}) do {:reply, {m, n}, {db, m, n}} end

	#these are low level functions for interactin with mailbox and ignoring channel manager
	def new(pub) do GenServer.cast(@name, {:new, pub}) end
	def del_acc(pub) do GenServer.cast(@name, {:del_acc, pub}) end
	def send(pub, message, price) do GenServer.cast(@name, {:send, pub, message, price}) end
	def del(pub, index) do GenServer.cast(@name, {:del, pub, index}) end
	def read(pub, index) do GenServer.call(@name, {:read, pub, index}) end
	def size(pub) do GenServer.call(@name, {:size, pub}) end
	def status do GenServer.call(@name, :status) end
	def test do
		MailBox.start_link
		MailBox.new("a")
		IO.puts(inspect MailBox.size("a"))
		MailBox.send("a", "hello", 0)
		IO.puts(inspect MailBox.size("a"))
		IO.puts(inspect MailBox.read("a", 0))
		MailBox.del("a", 0)		
		IO.puts(inspect MailBox.size("a"))
	end

	#these higher-level functions watch the channel manager to manage payments and permissions. 
	def register(tx) do if ChannelManager.accept(tx, Constants.registration_fee) do new(tx.data.pub) else "bad payment"	end end
	def delete_account(tx) do
		:Elixir.DeleteAccount = tx.data.__struct__
		foo = size(tx.data.pub)
		del_acc(tx.data.pub)
		bar = size(tx.data.pub)
		if foo != bar do
			ChannelManager.send(tx.data.pub, Constants.registration_fee * 9 / 10)
			#we should send a message to out peer about his new money.
		end
	end
	def cost do
		{_, messages} = status
		1000000*messages
	end
	def send(tx) do
		%Message{} = tx
		if ChannelManager.accept(tx.data.payment, cost) do send(tx.data.to, tx.data.msg, cost) else "bad payment"	end	end #what type is tx? channel_block?
	def delete(tx) do
		:Elixir.DeleteMessage = tx.data.__struct__
		msg = read(tx.data.pub, tx.data.index)
		time = :timer.now_diff(:os.timestamp, msg.time)
		two_days = 24*60*60*1000000
		del(tx.data.pub, tx.data.index) 
		after_msg = read(tx.data.pub, tx.data.index)
		cond do
			msg == after_msg -> "nothing to delete"
			true ->
				cost = msg.cost * ((two_days - time) / two_days)
				ChannelManager.send(tx.data.pub, cost)
				#we should send a message to out peer about his new money.
		end
	end
end

#what if every network message cost money?
#the node-node communication protocol could cost money.
