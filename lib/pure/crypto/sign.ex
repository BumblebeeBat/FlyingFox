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
    {:ok, sig} = Base.decode64(sig)
    {:ok, pub} = Base.decode64(pub)
    :crypto.verify(:ecdsa, :sha256, s, sig, [pub, params])
  end
  def sign_tx(tx, pub, priv) do
    h = DetHash.doit(tx)
    sig = sign(h, priv)
		m = %Meta{sig: sig}
    %CryptoSign{meta: m, data: tx}
  end
  def sign_tx_2(tx, pub, priv) do
		cb = tx.data
		cond do
			not (cb.pub2 == pub) -> IO.puts("not for me to sign")
			true ->
				h = DetHash.doit(tx.data)
				sig = sign(h, priv)
				m = %{tx.meta | sig2: sig}
				%CryptoSign{meta: m, data: tx.data}
		end
  end
  def verify_tx(tx) do
    #%{meta: meta, data: data} = signed_tx
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
