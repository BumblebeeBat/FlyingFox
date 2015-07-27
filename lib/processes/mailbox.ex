defmodule Msg do
	defstruct time: 0, msg: "", size: 0, price: 0, from: "", to: ""
end
#there should be a KV thread that this thread uses to store stuff. That way when this thread dies, the mailbox doesn't get dumped.
defmodule MailBox do
	#if you are running a node with external IP, then this thread could be generating profit for you.
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
			{:noreply, {HashDict.delete(db, pub), mailboxes - 1, messages - length(db[pub])}}
		end
	end
	def handle_cast({:send, to, message}, {db, m, messages}) do
		msg = %Msg{msg: message, time: Timer.stamp, size: byte_size(message[:msg]) + byte_size(message[:key]), price: messages*1000000, to: to}
		a = db[to]
		if a == nil do a = [] end
		dd = HashDict.put(db, to, a ++ [msg])
		{:noreply, {dd, m, messages+1}}
	end
	def handle_call({:pop, pub}, _from, {db, b, messages}) do
		if is_list(db[pub]) and length(db[pub]) > 0 do
			out = hd(db[pub])
			db = HashDict.put(db, pub, tl(db[pub]))#List.delete_at(db[pub], index))
		else
			out = nil
		end
		{:reply, out, {db, b, messages - 1}}	end
	def handle_call({:size, pub}, _from, {db, b, c}) do
		cond do
			db[pub] == nil -> out = 0
			true -> out = length(db[pub])
		end
		{:reply, out, {db, b, c}}
	end
	def handle_call(:status, _from, {db, m, n}) do {:reply, %{mailboxes: m, messages: n}, {db, m, n}} end
	def pop(pub) do
		msg = GenServer.call(@name, {:pop, pub})
		cond do
			msg == nil -> nil
			pub == Keys.pubkey -> nil
			true ->
				time = Timer.now_diff(msg.time) + 10000000 #10 second fee automatically
				two_days = 24*60*60*1000000
				refund = round(msg.price *  max(0, ((two_days - time) / two_days)))
				tx = ChannelManager.spend(pub, refund)
				%MsgPop{msg: msg, payment: tx}
		end
	end
	def size(pub) do GenServer.call(@name, {:size, pub}) end
	def status do GenServer.call(@name, :status) end
	def cost do cost(status) end
	def cost(s) do (1+s.messages)*1000000	end
	def accept(payment, cost, f) do if ChannelManager.accept(payment, cost) do f.() else "bad payment" end	end
	def register(payment, pub) do
		accept(payment, Constants.registration,  fn() ->
			GenServer.cast(@name, {:new, pub})
		end)
	end
	def send(payment, to, msg) do accept(payment, cost, fn() -> GenServer.cast(@name, {:send, to, msg}) end) end
	def delete_account(pub) do
		foo = size(pub)
		GenServer.cast(@name, {:del_acc, pub})
		bar = size(pub)
		if foo != bar do
			ChannelManager.send(pub, Constants.registration_fee * 9 / 10)
			#we should send a message to out peer about his new money.
		end
	end
end

#what if every network message cost money?
#the node-node communication protocol could cost money.
