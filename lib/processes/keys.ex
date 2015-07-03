defmodule Keys do
  use GenServer
  @name __MODULE__
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def init(:ok) do {:ok, {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="}} end
  def handle_cast(:new, x) do {:noreply, CryptoSign.new_key} end
  def handle_cast({:load, y, z}, x) do {:noreply, {y, z}} end
	def helper(m, x, f) do
    {pub, priv} = x
    sig=f.(m, pub, priv)
    {:reply, sig, x}
	end
  def handle_call({:sign, m}, _from, x) do helper(m, x, &(CryptoSign.sign_tx(&1, &2, &3))) end
  def handle_call(:pubkey, _from, x) do {:reply, elem(x, 0), x} end
  def pubkey do GenServer.call(@name, :pubkey) end
  def sign(m) do GenServer.call(@name, {:sign, m}) end
  def new do GenServer.cast(@name, :new) end
  def load(pub, priv) do GenServer.cast(@name, {:load, pub, priv}) end
  def master do load("BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0=") end
end
