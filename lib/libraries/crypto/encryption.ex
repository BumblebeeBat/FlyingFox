defmodule Encryption do
  #  1. eph_pub
	#  2. sym_enc( diffie_shared_key(eph_priv, receiver_pub), sender_pub || sign(sender_priv, eph_pub) || message )
	def si(key) do :crypto.stream_init(:rc4, :crypto.hmac(:sha256, "", key)) end
	def sym_enc(key, msg) do
    {_, x} = :crypto.stream_encrypt(si(key), PackWrap.pack(msg))
		Base.encode64(x)
	end
	def sym_dec(key, e_msg) do
		{:ok, b} = Base.decode64(e_msg)
		{_, x} = :crypto.stream_decrypt(si(key), b)
		x |> PackWrap.unpack
	end
	def send_msg(msg, to_pub) do
		{eph_pub, eph_priv} = CryptoSign.new_key
		%{key: eph_pub,
			msg: sym_enc(CryptoSign.shared_secret(to_pub, eph_priv),
									 %{from: Keys.pubkey,
										 sig: Keys.raw_sign(eph_pub),
										 msg: msg})}
	end
	def recieve_msg(msg) do
		sig = sym_dec(Keys.shared_secret(msg.key), msg.msg)
		if CryptoSign.verify(msg.key, sig[:sig], sig[:from]) do
			%{msg: sig[:msg], from: sig[:from], to: Keys.pubkey}
		else false end
	end
	def test do send_msg(%{test: :b, c: ["1234"], d: %Block{}}, Keys.pubkey) |> recieve_msg end
end
