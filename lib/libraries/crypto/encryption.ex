defmodule Encryption do
  #  1. eph_pub
	#  2. sym_enc( diffie_shared_key(eph_priv, receiver_pub), sender_pub || sign(sender_priv, eph_pub) || message )

	def diffie_shared_key(priv, pub) do CryptoSign.shared_secret(pub, priv) end
	def si(key) do
		key = :crypto.hmac(:sha256, "", key)
    b = <<182, 19, 103, 154, 8, 20, 217, 236, 119, 47, 149, 215, 120, 195, 95, 197>> 
		:crypto.stream_init(:aes_ctr, key, b)
	end
	def sym_enc(key, msg) do
    {_, x} = :crypto.stream_encrypt(si(key), PackWrap.pack(msg))
		Base.encode64(x)
	end
	def sym_dec(key, e_msg) do
		{:ok, b} = Base.decode64(e_msg)
		{_, x} = :crypto.stream_decrypt(si(key), b)
		x |> PackWrap.unpack
	end
	def send_msg(to_pub, msg) do
		{eph_pub, eph_priv} = CryptoSign.new_key
		sig = %{from: Keys.pubkey,
						sig: Keys.raw_sign(eph_pub),
						msg: msg}
		%{key: eph_pub,
			msg: sym_enc(diffie_shared_key(eph_priv, to_pub), sig)}
	end
	def recieve_msg(msg) do
		sig = sym_dec(Keys.shared_secret(msg.key), msg.msg)
		if CryptoSign.verify(msg.key, sig[:sig], sig[:from]) do
			%{msg: sig[:msg], from: sig[:from]}
		else
			false
		end
	end
	def test do
		send_msg(Keys.pubkey, "test") |> recieve_msg
	end
end
