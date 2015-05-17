defmodule PackWrap do
	def map2dict(m) do
		Map.to_list(m)
	end
	def dict2map(d) do
		b = d[:__struct__]
		d = Dict.delete(d, :__struct__)
		case b do
			"Elixir.Block" -> o = %Block{}
			"Elixir.Signed" ->
				o = %Signed{}
			"Elixir.Channel" -> o = %Channel{}
			"Elixir.RevealTx" -> o = %RevealTx{}
			"Elixir.SpendTx" -> o = %SpendTx{}
			"Elixir.SignTx" -> o = %SignTx{}
			"Elixir.Spend2WaitTx" -> o = %Spend2WaitTx{}
			"Elixir.Wait2BondTx" -> o = %Wait2BondTx{}
			"Elixir.Bond2SpendTx" -> o = %Bond2SpendTx{}
			"Elixir.SlasherTx" -> o = %SlasherTx{}
			"Elixir.ToChannelTx" -> o = %ToChannelTx{}
			"Elixir.ChannelBlockTx" -> o = %ChannelBlockTx{}
			"Elixir.CloseChannelTx" -> o = %CloseChannelTx{}
			"Elixir.Peer" -> o = %Peer{}
		end
		Dict.keys(d) |> Enum.map(&(o = Map.put(o, &1, d[&1])))
		o
	end
	def demap(o) do
		cond do
			is_list(o) -> Enum.map(o, &(demap(&1)))
			is_tuple(o) -> List.to_tuple(Enum.map(Tuple.to_list(o), &(demap(&1))))
			is_map(o) -> Enum.map(map2dict(o), &(demap(&1)))
			true -> o
		end
	end
	def pack(x) do x |> demap |> MessagePack.pack! end
  def slow_get(json, key) do#16,000 times in first 2 blocks.
    cond do
      json == [] -> nil
      true ->
        [{a, b}|tail] = json
        cond do
          a == key -> b
          true -> slow_get(tail, key)
        end
    end
  end
  def rekey(json) do
    cond do
      not is_list(json) or json == [] -> json
      is_tuple(hd(json))->
        Enum.map(Dict.keys(json), fn(k) ->
          {String.to_atom(k), rekey(slow_get(json, k))}
        end)
      true -> Enum.map(json, &(rekey(&1)))
    end
  end
	def remap(o) do
		cond do
			is_tuple(o) -> List.to_tuple(Enum.map(Tuple.to_list(o), &(remap(&1))))
			is_list(o) and length(o) > 0 and is_tuple(hd(o)) and :__struct__ in Dict.keys(o) ->
				dict2map(Enum.map(o, &(remap(&1))))
			is_list(o) and length(o) > 0 -> o |> Enum.map(&(remap(&1)))
			true -> o
		end
	end
  def unpack(x) do
		out = rekey(MessagePack.unpack!(x))
		remap(out)
		out
	end
  def test do
    x = unpack(pack(%Block{}))
    IO.puts inspect x
  end
end

