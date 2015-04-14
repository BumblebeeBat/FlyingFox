defmodule DetHash do
  def hash(s) do :crypto.hmac(:sha256, to_string(s), "") |> Base.encode64 end
  def collect(l) do Enum.reduce(l, "", fn element, acc -> element<>acc end) end #list of strings -> string
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
      is_tuple(a) -> hash_list(Tuple.to_list(a))
      is_integer(a) -> hash(a)
      is_float(a) -> hash(a)
      is_binary(a) -> hash(a)
      true -> hash_dict(a)
    end
  end
  def test() do IO.puts(doit([a: :b])) end
end

