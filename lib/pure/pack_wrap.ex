defmodule PackWrap do
	def map2dict(m) do Map.to_list(m)	end
	def dict2map(d) do
		b = d[:__struct__]
		d = Dict.delete(d, :__struct__)
		case b do
			"Elixir.Block" -> o = %Block{}
			"Elixir.Signed" -> o = %Signed{}
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
		Enum.reduce(Dict.keys(d), o, &(Map.put(&2, &1, d[&1])))
	end
	def demap(o) do #changes all maps in a datastructure into tuple lists representations.
		cond do
			is_list(o) -> Enum.map(o, &(demap(&1)))
			is_tuple(o) -> List.to_tuple(Enum.map(Tuple.to_list(o), &(demap(&1))))
			is_map(o) -> Enum.map(map2dict(o), &(demap(&1)))
			true -> o
		end
	end
	def remap(o) do #changes all the tuple list representations in the datastructure back into maps.
		cond do
			is_tuple(o) -> List.to_tuple(Enum.map(Tuple.to_list(o), &(remap(&1))))
			is_list(o) and length(o) > 0 and is_tuple(hd(o)) and :__struct__ in Dict.keys(o) ->
				o |> Enum.map(&(remap(&1))) |> dict2map
			is_list(o) and length(o) > 0 -> o |> Enum.map(&(remap(&1)))
			true -> o
		end
	end
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
	#def pack(x) do x |> demap |> MessagePack.pack! |> Base.encode64 end
  #def unpack(x) do x |> Base.decode64 |> MessagePack.unpack! |> rekey |> remap	end
	def pack(x) do x |> demap |> MessagePack.pack! end
  def unpack(x) do
		case MessagePack.unpack(x) do
			{:ok, y} -> y |> rekey |> remap
			{:error, :incomplete} -> 1=3
			{a, b}  ->
				IO.puts(" Failed to unpack. #{inspect x}")
				IO.puts("#{inspect a} #{inspect b}")
		end
	end
	#def pack(x) do x |> demap |> :jiffy.encode end
  #def unpack(x) do
	#	:jiffy.decode(x) |> rekey |> remap 
	#end
	def test do
    x = unpack(pack(%Block{}))
    IO.puts inspect x
  end
	def test2 do remap([[__struct__: "Elixir.Signed", data: [__struct__: "Elixir.SignTx", height: 1, nonce: 0, prev_hash: "Dh9J/r3hhi9ems8FAE6gK4uMgEsbMiiKF6CIwyOPe+w=", secret_hash: "Urq9fr3KVUhU3+tHbk0EnYOqlk3UgD/qEuRijHr/ckM=", winners: [2, 6, 12, 17, 19, 23, 24, 26, 36, 38, 39, 40, 41, 45, 48, 49, 57, 62, 66, 68, 69, 72, 76, 78, 79, 85, 87, 88, 91, 93, 95, 97, 100, 104, 118, 121, 130, 131, 149, 151, 152, 154, 157]], meta: [], pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", sig: "MEQCIFOdKYCUdbxFh9fq8jQoBi6DobWHakGbjSLC54tgXWtWAiBothy0S0UKaR9fBJJPZQ5Az3+IPemwEyfsAKtol0pK/g=="]]) end
end

OB
