#these variables can be different on every node
defmodule Keys do
  use GenServer
  def key do :address end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, {"", ""}} end
  def handle_cast(:new, x) do {:noreply, Sign.new_key} end
  def handle_cast({:load, y, z}, x) do {:noreply, {y, z}} end
  def handle_call({:sign, m}, _from, x) do
    {pub, priv} = x
    sig=Sign.sign_tx(m, pub, priv)
    {:reply, sig, x}
  end
  def handle_call(:pubkey, _from, x) do {:reply, elem(x, 0), x} end
  def pubkey do GenServer.call(key, :pubkey) end
  def sign(m) do GenServer.call(key, {:sign, m}) end
  def new do GenServer.cast(key, :new) end
  def load(pub, priv) do GenServer.cast(key, {:load, pub, priv}) end
  def master do load("BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0=") end
end
