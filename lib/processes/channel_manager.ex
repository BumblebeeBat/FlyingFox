#channel manager needs to keep track of the highest-nonced transaction from the peer, and it also needs to keep track of the highest-nonced transaction we sent to the peer.
#eventually we need to store multiple hash_locked transactions from the peer.
defmodule ChannelManager do
	defstruct recieved: %CryptoSign{data: %ChannelBlock{}}, sent: %CryptoSign{data: %ChannelBlock{}}#, hash_locked: []
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do   GenServer.start_link(__MODULE__, %HashDict{}, name: @name) end
	def get(pub) do       GenServer.call(@name, {:get, pub}) end
  def get_all do        GenServer.call(@name, :get_all) end
  def handle_call(:get_all, _from, mem) do {:reply, mem, mem} end
  def handle_call({:get, pub}, _from, mem) do
		out = mem[pub]
		if out == nil do
			out = %ChannelManager{sent: %CryptoSign{data: %ChannelBlock{pub: Keys.pubkey, pub2: pub}}}
		end
		{:reply, out, mem} end
  def handle_cast({:send, pub, channel},  mem) do
		out = mem[pub]
		if out == nil do out = %ChannelManager{} end
		out = %{out | sent: channel}
		{:noreply, HashDict.put(mem, pub, out)}
	end
	def handle_cast({:recieve, pub, channel},  mem) do
		out = mem[pub]
		if out == nil do out = %ChannelManager{} end
		out = %{out | recieved: channel}
		{:noreply, HashDict.put(mem, pub, out)}
	end
	def other(tx) do
		tx = [tx.data.pub, tx.data.pub2] |> Enum.filter(&(&1 != Keys.pubkey))
		cond do
			tx = [] -> Keys.pubkey
			true -> hd(tx)
		end
	end
	def accept(tx, min_amount, mem \\ []) do
		if accept_check(tx, min_amount, mem) do
			other = other(tx)
			GenServer.cast(@name, {:recieve, other, tx})
			d = 1
			if tx.data.pub2 == Keys.pubkey do d = d * -1 end
			tx.data.amount * d
		else
			IO.puts("bad channel #{inspect tx}")
		end			
	end
	def accept_check(tx, min_amount \\ -Constants.initial_coins, mem \\ []) do
		other = other(tx)
		d = -1
		if Keys.pubkey == tx.data.pub do d = d * -1 end
		if mem != [] do x = mem[other] else x = get(other) end
		x = x |> top_block
		cond do
			d == 1 and tx.meta.sig2 == nil ->
				IO.puts("2 should be signed by partner")
				false
			d == -1 and tx.meta.sig == nil ->
				IO.puts("should be signed by partner")
				false
			not (tx.data.amount - x.data.amount > min_amount) ->
				IO.puts("didn't spend enough")
				false
			not (:Elixir.CryptoSign == tx.__struct__) ->
				IO.puts("unsigned")
				false
			not (:Elixir.ChannelBlock == tx.data.__struct__) ->
				IO.puts("isn't a channel block")
				false
			not ChannelBlock.check(Keys.sign(tx)) ->
				IO.puts("isn't a valid channel block")
				false
			not (tx.data.nonce > x.data.nonce) ->
				IO.puts("nonce is too low")
				false#eventually we should keep the biggest spendable block, and all bigger ones.
			true -> true
		end
	end
	def top_block(c) do
		[c.sent, c.recieved]
		|> Enum.sort(&(&1.data.nonce > &2.data.nonce))
		|> hd
	end
	def spend(pub, amount, default \\ Constants.default_channel_balance) do
		IO.puts("pub #{inspect pub}")
		cb = get(pub) |> top_block
		IO.puts("cb #{inspect cb}")
		cb = cb.data
		IO.puts("cb #{inspect cb}")
		if is_binary(amount) do amount = String.to_integer(amount) end
		on_chain = KV.get(ToChannel.key(pub, Keys.pubkey))
		IO.puts("on_chain #{inspect on_chain}")
		l = [cb.pub, cb.pub2]
		if ((not (Keys.pubkey in l)) or not (pub in l)) do cb = %{cb | pub: Keys.pubkey, pub2: pub} end
		if not pub in [cb.pub, cb.pub2] do cb = %{cb | pub2: Keys.pubkey} end
		if Keys.pubkey != cb.pub do amount = -amount end
		cb = %{cb | amount: cb.amount + amount, nonce: cb.nonce + 1} |> Keys.sign
		if on_chain.amount <= amount or on_chain.amount2 <= -amount do
			IO.puts("not enough money in the channel to spend that much")
			IO.puts("on chain #{inspect on_chain}")
			IO.puts("amount #{inspect amount}")
		else
			IO.puts("channel manager spend #{inspect cb}")
			GenServer.cast(@name, {:send, pub, cb})
		end
		cb
	end
end

