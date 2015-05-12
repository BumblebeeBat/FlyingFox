defmodule DetHash do
  def collect(l) do Enum.reduce(l, "", fn element, acc -> element<>acc end) end #list of strings -> string
  def hash_list(l) do Enum.sort(l) |> Enum.map(&(det_string(&1))) |> collect |> det_string end
  def map_hash(s) do
    keys = Map.keys(s) |> Enum.sort()
    values = Enum.map(keys, fn x -> Map.get(s, x) end)
    hash_list(keys ++ values)
  end
  def det_string(a) do
    cond do
      is_list(a) -> hash_list(a)
      is_tuple(a) -> hash_list(Tuple.to_list(a))
      is_integer(a) -> inspect(a)
      is_float(a) -> inspect(a)
      is_binary(a) -> inspect(a)
      is_map(a) -> map_hash(a)
      is_atom(a) -> inspect(a)
      true -> 
        IO.puts("error unsuported type #{inspect a}")
        1=2
    end
  end
  def hash(s) do :crypto.hmac(:sha256, to_string(s), "") |> Base.encode64 end
  def doit(a) do det_string(a) |> hash end
  def test() do 
    {p, q} = Sign.new_key
    s=Sign.sign_tx([], p, q)
    DetHash.doit(s)
  end
end

