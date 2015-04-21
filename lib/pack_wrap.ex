defmodule PackWrap do
  def pack(x) do MessagePack.pack!(x) end
  def slow_get(json, key) do#16,000 times in first 2 blocks.
    cond do
      json == [] -> nil
      true ->
        [{a, b}|tail]=json
        cond do
          a==key -> b
          true -> slow_get(tail, key)
        end
    end
  end
  def rekey(json) do
    cond do
      not is_list(json) or json==[] -> json
      is_tuple(hd(json))->
        Enum.map(Dict.keys(json), fn(k) ->
          {String.to_atom(k), rekey(slow_get(json, k))}
        end)
      true -> Enum.map(json, &(rekey(&1)))
    end
  end
  def unpack(x) do rekey(MessagePack.unpack!(x)) end
  def test do
    x=unpack(pack([meta: :b]))
    IO.puts inspect x
  end
end

