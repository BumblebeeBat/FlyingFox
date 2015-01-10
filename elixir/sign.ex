defmodule Sign do
	def params do
		:crypto.ec_curve(:secp256k1)
	end
	def new_key do
		{pub, priv}=:crypto.generate_key(:ecdh, params)
		pub=Base.encode64(pub)
		priv=Base.encode64(priv)
		{pub, priv}
	end
	def sign(s, priv) do
		{:ok, priv} = Base.decode64(priv)
		:crypto.sign(:ecdsa, :sha256, s, [priv, params]) |>
		Base.encode64
	end
	def verify(s, sig, pub) do
		{:ok, sig} = Base.decode64(sig)
		{:ok, pub} = Base.decode64(pub)
		:crypto.verify(:ecdsa, :sha256, s, sig, [pub, params])
	end
	def sign_tx(tx, pub, priv) do
		h=Det_hash.hash_dict(tx)
		sig=sign(h, priv)
		sig=sig
		{pub, sig, tx}
	end
	def verify_tx(signed_tx) do
		{pub, sig, tx}=signed_tx
		h=Det_hash.hash_dict(tx)
		verify(h, sig, pub)
	end
	def test do
		{pub, priv}=new_key
		tx=[a: "b", b: "c"]
		tx=sign_tx(tx, pub, priv) 
		true=verify_tx(tx)
		s="test string"
		sig=sign(s, priv)
		true=verify(s, sig, pub)
		false=verify(s<>" ", sig, pub)
		"success"
	end
end
