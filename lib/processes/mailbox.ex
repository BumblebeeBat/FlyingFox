defmodule Msg do
	defstruct time: 0, msg: "", size: 0, price: 0, from: ""
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
	def handle_cast({:send, to, message, from}, {db, m, messages}) do
		msg = %Msg{msg: message, time: :os.timestamp, size: byte_size(message), price: messages*1000000, from: from}
		dd = HashDict.put(db, to, [msg|db[to]])
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
	def handle_call(:status, _from, {db, m, n}) do {:reply, %{mailboxes: m, messages: n}, {db, m, n}} end
	def read(pub, index) do GenServer.call(@name, {:read, pub, index}) end
	def size(pub) do GenServer.call(@name, {:size, pub}) end
	def status do GenServer.call(@name, :status) end
	def cost do cost(status) end
	def cost(s) do s.messages*1000000	end
	def accept(payment, cost, f) do if ChannelManager.accept(payment, cost) do f.() else "bad payment" end	end
	def register(payment, pub) do accept(payment, Constants.registration,  fn() -> GenServer.cast(@name, {:new, pub}) end) end
	def send(payment, to, msg, from) do accept(payment, cost, fn() -> GenServer.cast(@name, {:send, to, msg, from}) end) end
	def delete_account(pub) do
		foo = size(pub)
		GenServer.cast(@name, {:del_acc, pub})
		bar = size(pub)
		if foo != bar do
			ChannelManager.send(pub, Constants.registration_fee * 9 / 10)
			#we should send a message to out peer about his new money.
		end
	end
	def delete(pub, index) do
		msg = read(pub, index)
		time = :timer.now_diff(:os.timestamp, msg.time)
		two_days = 24*60*60*1000000
		GenServer.cast(@name, {:del, pub, index})
		after_msg = read(pub, index)
		cond do
			msg == after_msg -> "nothing to delete"
			true ->
				cost = msg.cost * ((two_days - time) / two_days)
				ChannelManager.send(pub, cost)
				#we should send a message to out peer about his new money.
		end
	end
end

#what if every network message cost money?
#the node-node communication protocol could cost money.
