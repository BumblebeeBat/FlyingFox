defmodule Meta do
	defstruct sig: nil, sig2: nil, revealed: []
end

defmodule CryptoSign do
  defstruct data: nil, meta: %Meta{} #sig and sig2 go in meta
  def params do
    :crypto.ec_curve(:secp256k1)
  end
	def shared_secret(pub, priv) do
		:crypto.compute_key(:ecdh, elem(Base.decode64(pub), 1), elem(Base.decode64(priv), 1), params)
	end
  def new_key do
    {pub, priv} = :crypto.generate_key(:ecdh, params)
    pub = Base.encode64(pub)
    priv = Base.encode64(priv)
    {pub, priv}
  end
  def sign(s, priv) do
    {:ok, priv} = Base.decode64(priv)
    :crypto.sign(:ecdsa, :sha256, s, [priv, params]) 
    |> Base.encode64
  end
  def verify(s, sig, pub) do
    {x, sig} = Base.decode64(sig)
    {y, pub} = Base.decode64(pub)
		cond do
			x != :ok -> false
			y != :ok -> false
			true -> :crypto.verify(:ecdsa, :sha256, s, sig, [pub, params])
		end
  end
  def sign_tx(tx, pub, priv) do
		if :__struct__ in Map.keys(tx) and tx.__struct__ == :Elixir.CryptoSign do
			cb = tx.data
			m = tx.meta
		else
			cb = tx
			m = %Meta{}
		end
		h = DetHash.doit(cb)
		sig = sign(h, priv)
		a = true
		if cb.pub == pub do
				m = %{m | sig: sig}
				a = false
		end
		if :pub2 in Map.keys(cb) and (cb.pub2 == pub) do
				m = %{m | sig2: sig}
				a = false
		end
		if a do IO.puts("not for me to sign") end
		%CryptoSign{meta: m, data: cb}
  end
  def verify_tx(tx) do
    h = DetHash.doit(tx.data)
		pub = tx.data.pub
    verify(h, tx.meta.sig, pub)
  end
  def test do
    {pub, priv} = new_key
    tx = %{pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc="}
    tx = sign_tx(tx, pub, priv) 
    true = verify_tx(tx)
    s = "test string"
    sig = sign(s, priv)
    true = verify(s, sig, pub)
    false = verify(s <> " ", sig, pub)
    "success"
  end
	def check_sig2(tx) do
		h = DetHash.doit(tx.data)
		pub = tx.data.pub2
		verify(h, tx.meta.sig2, pub)
	end
end
