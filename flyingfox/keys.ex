#these variables can be different on every node
defmodule Keys do
  def looper(x) do
    receive do
      ["pubkey", s] -> send(s, elem(x, 0))
      ["sign", m, s] ->
        {pub, priv} = x
        sig=Sign.sign_tx(m, pub, priv)
        send(s, sig)
      ["new"] -> x = Sign.new_key
      ["load", y] -> x = y
    end
    looper(x)
  end
  def key do :address end
  def pubkey do
    send(key, ["pubkey", self()])
    receive do x -> x end
  end
  def load(keys) do 
    send(key, ["load", keys])
    Blockchain.sign_reveal
  end
  def sign(x) do
    send(key, ["sign", x, self()])
    receive do x -> x end
  end
  def new do send(key, ["new"]) end
  def master_keys do {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="} end#{pub, priv}
  def master do load(master_keys) end
  def start do
    {:ok, pid}=Task.start_link(fn -> looper({}) end)
    Process.register(pid, key)
    new
  end
end
