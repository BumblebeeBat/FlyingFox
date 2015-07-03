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
			out = %CryptoSign{data: %ChannelBlock{}}
		end
		{:reply, out, mem} end
  def handle_cast({:update, pub, channel},  mem) do
		is_integer(accept(channel)) = true
		pub in [channel.data.pub, channel.data.pub2] = true
		pub != Keys.pubkey = true
		{:noreply, HashDict.put(mem, pub, channel)}
	end
	def del(pub) do update(pub, nil) end	

	def accept(tx, min_amount) do
		other = [tx.data.pub, tx.data.pub2] |> Enum.filter(&(&1 != Keys.pubkey)) |> hd
		channel = get(other)
		d = -1
		if Keys.pubkey == tx.data.pub do d = d * -1 end
		cond do
			not tx.data.amount > min_amount -> false
			not :Elixir.CryptoSign = tx.__struct__ -> false
			not :Elixir.ChannelBlock = tx.data.__struct__ -> false
			not ChannelBlock.check(tx) -> false
			not CryptoSign.verify_tx(tx, []) -> false
			not tx.data.nonce > mem[pub].data.nonce -> false#should keep the biggest spendable block, and all bigger ones.
			true ->
				update(other, tx)
				(tx.data.amount - tx.data.amount) * d
		end
	end
	def spend(pub, amount, default \\ Constants.default_channel_balance) do
		channel = mem[pub]
		if channel == nil do
			channel = KV.get(ToChannel.key(pub, Keys.pubkey))
		end
		if channel == nil do
			TxCreator.to_channel(pub, amount+default)
			IO.puts("wait till next block, and try again")
		end
		a = channel.amount
		if Keys.pubkey == channel.pub do
			a = channel.amount2
		end
		if a<amount do
			up = amount - a + default
			TxCreator.to_channel(pub, up)
			IO.puts("wait till next block, and try again")
		end
		c = KV.get(ToChannel.key(Keys.pubkey, other))
		if c == nil do
			IO.puts("this channel doesn't exist yet, so you cannot close it.")
		end
		me = Keys.pubkey
		d = 1
		if me == c.pub do
			d = -1
		end
		channel = get(other)
		new = %{c | amount: d * amount, nonce: channel.nonce+1}
		|> Keys.sign(new)
		ChannelManager.update(other, new)
		new
	end
end
