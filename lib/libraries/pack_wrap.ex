defmodule PackWrap do
	def map2dict(m) do Map.to_list(m)	end
	def dict2map(d) do
		if not (is_list(d) and Dict.has_key?(d, :__struct__)) do
			d
		else
			b = d[:__struct__]
			d = Dict.delete(d, :__struct__)
			case b do
				"Elixir.Block" -> o = %Block{}
				"Elixir.CryptoSign" -> o = %CryptoSign{}
				"Elixir.Reveal" -> o = %Reveal{}
				"Elixir.Spend" -> o = %Spend{}
				"Elixir.Sign" -> o = %Sign{}
				"Elixir.Spend2Wait" -> o = %Spend2Wait{}
				"Elixir.Wait2Bond" -> o = %Wait2Bond{}
				"Elixir.Bond2Spend" -> o = %Bond2Spend{}
				"Elixir.Slasher" -> o = %Slasher{}
				"Elixir.ToChannel" -> o = %ToChannel{}
				"Elixir.ChannelBlock" -> o = %ChannelBlock{}
				"Elixir.CloseChannel" -> o = %CloseChannel{}
				"Elixir.Peer" -> o = %Peer{}
				"Elixir.Meta" -> o = %Meta{}
				"Elixir.Status" -> o = %Status{}
				"Elixir.ChannelManager" -> o = %ChannelManager{}
				"Elixir.DeleteAccount" -> o = %DeleteAccount{}
				"Elixir.SendMessage" -> o = %SendMessage{}
				"Elixir.PopMessage" -> o = %PopMessage{}
				"Elixir.InboxSize" -> o = %InboxSize{}
				"Elixir.Msg" -> o = %Msg{}
				"Elixir.Account" -> o = %Account{}
				nil -> o = %{}
				x -> IO.puts("dict2map odd ball #{inspect x}")
			end
			Enum.reduce(Dict.keys(d), o, &(Map.put(&2, &1, d[&1])))
		end
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
		#IO.puts("o #{inspect o}")
		cond do
			is_tuple(o) and is_list(elem(o, 0)) -> remap(elem(o, 0))#test
			is_tuple(o) -> List.to_tuple(Enum.map(Tuple.to_list(o), &(remap(&1))))
			is_list(o) and is_tuple(hd(o)) and is_list(elem(hd(o), 0)) ->
			  Enum.map(o, &(remap(&1)))
			is_list(o) and length(o) > 0 and is_tuple(hd(o)) and "__struct__" in Dict.keys(o) ->
				o |> rekey |> remap
			#is_list(o) and length(o) > 0 and is_tuple(hd(o)) and :__struct__ in Dict.keys(o) ->#old can be removed.
			#o |> Enum.map(&(remap(&1))) |> dict2map#this was commented out...
			is_list(o) and length(o) > 0 -> o |> Enum.map(&(remap(&1)))
			o == "nil" -> nil
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
			is_tuple(json) ->
				rekey(elem(json, 0)) |> dict2map
			json == "nil" -> nil
      not is_list(json) or json == [] -> json
      is_tuple(hd(json)) and is_list(elem(hd(json), 0)) ->
				Enum.map(json, &(rekey(&1)))
			is_tuple(hd(json)) ->
        Enum.map(Dict.keys(json), fn(k) ->
          {String.to_atom(k), rekey(slow_get(json, k))}
        end)
      true -> Enum.map(json, &(rekey(&1)))
    end
  end
	def pack(x) do x |> :jiffy.encode end
  def unpack(x) do
		if x == "" do "" else	:jiffy.decode(x) |> rekey |> dict2map	end
	end
	def test do
		#t = %CloseChannel{}
		#t = :status
		#t = [1,2,3]
		t = %HashDict{}
		t = HashDict.put(t, "a", "b")
    x = unpack(pack(t))
    IO.puts inspect x
  end
	def test2 do remap([[__struct__: "Elixir.CryptoSign", data: [__struct__: "Elixir.Sign", height: 1, nonce: 0, prev_hash: "Dh9J/r3hhi9ems8FAE6gK4uMgEsbMiiKF6CIwyOPe+w=", secret_hash: "Urq9fr3KVUhU3+tHbk0EnYOqlk3UgD/qEuRijHr/ckM=", winners: [2, 6, 12, 17, 19, 23, 24, 26, 36, 38, 39, 40, 41, 45, 48, 49, 57, 62, 66, 68, 69, 72, 76, 78, 79, 85, 87, 88, 91, 93, 95, 97, 100, 104, 118, 121, 130, 131, 149, 151, 152, 154, 157]], meta: [], pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", sig: "MEQCIFOdKYCUdbxFh9fq8jQoBi6DobWHakGbjSLC54tgXWtWAiBothy0S0UKaR9fBJJPZQ5Az3+IPemwEyfsAKtol0pK/g=="]]) end
end


