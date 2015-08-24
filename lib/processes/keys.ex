defmodule Keys do
	#this module is designed to make it impossible to extract the private key.
	#unfortunately, that means that if this file crashes, your private key gets deleted.
	#make sure you back up your private key.
  use GenServer
  @name __MODULE__
	def db_location do "/keys" end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(:ok) do
		x = DB.get_raw(db_location)
		if x == "" do
			DB.put_function(db_location, fn() -> %{pub: nil, priv: nil}	end)
			{:ok, {nil, nil}}
		else
			{:ok, {x.pub, nil}}
		end
	end
	def store(pub, priv, brainwallet) do DB.put_function(db_location, fn() -> %{pub: pub, priv: Encryption.sym_enc(brainwallet, priv)}	end) end
  def handle_cast({:load, pub, priv, brainwallet}, x) do
		#store on harddrive. If you already have a pub/priv pair loaded, it will NOT replace it.
		x = DB.get_raw(db_location)
		if x.priv == nil do	store(pub, priv, brainwallet) end
		{:noreply, {pub, priv}}
	end
	def handle_cast({:unlock, brainwallet}, _) do
		x = DB.get_raw(db_location)
		priv = Encryption.sym_dec(brainwallet, x.priv)
		{:noreply, {x.pub, priv}}
	end
	def handle_cast(:lock, {pub, priv}) do {:noreply, {pub, nil}}	end
	def handle_cast({:change_password, current, new}, {pub, priv}) do
		x = DB.get_raw(db_location)
		msg = "abc"
		cond do 
			x == "" -> {:noreply, {pub, priv}}
			x.priv == nil ->
				{pub, priv} = CryptoSign.new_key
				store(pub, priv, new)
				{:noreply, {pub, priv}}
			true ->
				new_priv = Encryption.sym_dec(current, x.priv)
				msg = "abc"
				if CryptoSign.verify(msg, CryptoSign.sign(msg, new_priv), x.pub) do
					priv = new_priv
					pub = x.pub
					store(pub, priv, new)
				end
				{:noreply, {pub, priv}}
		end
	end
	def handle_call({:ss, pub}, _from, x) do
    {_, priv} = x
		out = CryptoSign.shared_secret(pub, priv)
		{:reply, out, x}
	end
  def handle_call({:raw_sign, m}, _from, x) do
    {_, priv} = x
		cond do
			not is_binary(m) -> sig = "not binary"
			priv == nil -> sig = "need passphrase to unlock"
			true -> sig = CryptoSign.sign(m, priv)
		end
    {:reply, sig, x}
	end
  def handle_call({:sign, m}, _from, x) do
    {pub, priv} = x
		cond do
			not is_map(m) -> sig = "bad tx"
			priv == nil -> sig = "need passphrase to unlock"
			true -> sig = CryptoSign.sign_tx(m, pub, priv)
		end
    {:reply, sig, x}
	end
	def handle_call(:status, _from, x) do
		{_, priv} = x
		y = DB.get_raw(db_location)
		cond do
			y.priv == nil -> out = :empty
			priv == nil -> out = :locked
			true -> out = :unlocked
		end
		{:reply, out, x}
	end
  def handle_call(:pubkey, _from, x) do {:reply, elem(x, 0), x} end
  def pubkey do GenServer.call(@name, :pubkey) end
  def sign(m) do GenServer.call(@name, {:sign, m}) end
  def raw_sign(m) do GenServer.call(@name, {:raw_sign, m}) end
  def load(pub, priv, brainwallet) do GenServer.cast(@name, {:load, pub, priv, brainwallet}) end
  def unlock(brainwallet) do GenServer.cast(@name, {:unlock, brainwallet}) end
  def lock do GenServer.cast(@name, :lock) end
	def status do GenServer.call(@name, :status) end
	def change_password(current, new) do GenServer.cast(@name, {:change_password, current, new}) end
  def new(brainwallet) do change_password("", brainwallet) end
	def shared_secret(pub) do GenServer.call(@name, {:ss, pub}) end
  #def master do load("BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0=") end
end

