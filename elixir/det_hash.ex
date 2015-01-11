defmodule Det_hash do
  def hash(s) do
    :crypto.hmac(:sha256, to_string(s), "") |> Base.encode16 end
  def collect(l) do#list of strings -> string
    Enum.reduce(l, "", fn element, acc -> element<>acc end) end
  def hash_list(l) do
    hash(collect(Enum.map(Enum.sort(l), fn x -> inspect(x) end))) end
  def hash_dict(s) do
    keys = Dict.keys(s)
    keys = Enum.sort(keys)
    values = Enum.map( keys, fn x -> elem(Dict.fetch(s, x), 1) end)
    hash_list( keys ++ values )
  end
  def doit(a) do
    cond do
      is_list(a) -> hash_list(a)
      is_integer(a) -> hash(a)
      is_float(a) -> hash(a)
      true -> hash_dict(a)
    end
  end
  def test() do
    d = HashDict.new
    d = Dict.put(d, "a", "b")
    d = Dict.put(d, "e", "b")
    d = Dict.put(d, "f", "b")
    d = Dict.put(d, "c", "b")
    d = Dict.put(d, "b", "b")
    d = Dict.put(d, "d", "b")
    IO.puts(Det_hash.doit(d))#8A667238A57A1B42E3AC5CC14A97C25F4F2D90EE32148EB2EC4780B6FC2190A2
  end
end

