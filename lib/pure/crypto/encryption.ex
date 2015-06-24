defmodule Encryption do
  def times(x, f, n) do
    cond do
      n<1 -> x
      true -> times(f.(x), f, n-1)
    end
  end
  def si(key) do
    f = fn(x) -> :crypto.hmac(:sha256, "", x) end
    key = key |> times(f, 2000000)
    b = <<182, 19, 103, 154, 8, 20, 217, 236, 119, 47, 149, 215, 120, 195, 95, 197>> 
    si = :crypto.stream_init(:aes_ctr, key, b)
  end
  def encrypt(x, password) do 
    {_, x} = :crypto.stream_encrypt(si(password), x)
    x |> Base.encode64 
  end
  def decrypt(x, password) do 
    {:ok, b} = Base.decode64(x)
    {_, out} = :crypto.stream_decrypt(si(password), b)
    out
  end
	def normal(l) do Enum.reduce(tl(l), hd(l), &(&2 <> " " <> &1))end
	def ss(args) do CryptoSign.shared_secret(hd(args), hd(tl(args))) end
	def ss_encrypt(args) do
		b = normal(tl(tl(args)))
		c = [hd(args), hd(tl(args))]
		Encryption.encrypt(b, ss(c))
	end
	def ss_decrypt(args) do
		Encryption.decrypt(hd(tl(tl(args))), ss(args))
	end
end
