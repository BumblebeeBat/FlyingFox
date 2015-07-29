defmodule Keys do
	#this module is designed to make it impossible to extract the private key.
	#unfortunately, that means that if this file crashes, your private key gets deleted.
	#make sure you back up your private key.
  use GenServer
  @name __MODULE__
	def db_location do "/keys" end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(:ok) do
		pub = Leveldb.get("pub", db_location)
		IO.puts("keys pub #{inspect pub}")
		cond do
			pub == :not_found -> {:ok, {nil, nil}}
			true -> {:ok, {pub, nil}}
		end
	end
  def handle_cast({:new, brainwallet}, x) do
		{pub, priv} = CryptoSign.new_key
		IO.puts("new key #{inspect pub}")
		Leveldb.put("pub", pub, db_location)
		Leveldb.put("priv", Encryption.sym_enc(brainwallet, priv), db_location)
		{:noreply, {pub, priv}}
	end
  def handle_cast({:load, pub, priv, brainwallet}, x) do
		#store on harddrive
		Leveldb.put("pub", pub, db_location)
		Leveldb.put("priv", Encryption.sym_enc(brainwallet, priv), db_location)
		{:noreply, {pub, priv}}
	end
	def handle_cast({:unlock, brainwallet}, x) do
		{pub, _} = x
		priv = Leveldb.get("priv", db_location)
		priv = Encryption.sym_dec(brainwallet, priv)
		{:noreply, {pub, priv}}
		end
	def handle_cast({:lock}, {pub, priv}) do {:noreply, {pub, nil}}	end
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
		cond do
			priv == nil -> out = :locked
			true -> out = :unlocked
		end
		{:reply, out, x}
	end
  def handle_call(:pubkey, _from, x) do {:reply, elem(x, 0), x} end
  def pubkey do GenServer.call(@name, :pubkey) end
  def sign(m) do GenServer.call(@name, {:sign, m}) end
  def raw_sign(m) do GenServer.call(@name, {:raw_sign, m}) end
  def new(brainwallet) do GenServer.cast(@name, {:new, brainwallet}) end
  def load(pub, priv, brainwallet) do GenServer.cast(@name, {:load, pub, priv, brainwallet}) end
  def unlock(brainwallet) do GenServer.cast(@name, {:unlock, brainwallet}) end
  def lock do GenServer.cast(@name, {:lock}) end
	def status do GenServer.call(@name, {:status}) end
	def shared_secret(pub) do GenServer.call(@name, {:ss, pub}) end
  #def master do load("BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0=") end
end

