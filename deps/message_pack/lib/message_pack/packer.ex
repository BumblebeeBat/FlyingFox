defmodule MessagePack.Packer do
  @spec pack(term) :: { :ok, binary } | { :error, term }
  @spec pack(term, Keyword.t) :: { :ok, binary } | { :error, term }
  def pack(term, options \\ []) do
    options = parse_options(options)

    case do_pack(term, options) do
      { :error, _ } = error ->
        error
      packed ->
        { :ok, packed }
    end
  end

  @spec pack!(term) :: binary | no_return
  @spec pack!(term, Keyword.t) :: binary | no_return
  def pack!(term, options \\ []) do
    case pack(term, options) do
      { :ok, packed } ->
        packed
      { :error, error } ->
        raise ArgumentError, message: inspect(error)
    end
  end

  defp parse_options(options) do
    enable_string = !!options[:enable_string]

    {packer, unpacker} = case options[:ext] do
      nil ->
        { nil, nil }
      mod when is_atom(mod) ->
        { &mod.pack/1, &mod.unpack/2 }
      list when is_list(list) ->
        { list[:packer], list[:unpacker] }
    end

    %{enable_string: enable_string, ext_packer: packer, ext_unpacker: unpacker}
  end

  defp do_pack(nil, _),   do: << 0xC0 :: size(8) >>
  defp do_pack(false, _), do: << 0xC2 :: size(8) >>
  defp do_pack(true, _),  do: << 0xC3 :: size(8) >>
  defp do_pack(atom, options) when is_atom(atom), do: do_pack(Atom.to_string(atom), options)
  defp do_pack(i, _) when is_integer(i) and i < 0, do: pack_int(i)
  defp do_pack(i, _) when is_integer(i), do: pack_uint(i)
  defp do_pack(f, _) when is_float(f), do: << 0xCB :: size(8), f :: size(64)-big-float-unit(1)>>
  defp do_pack(binary, %{enable_string: true}) when is_binary(binary) do
    if String.valid?(binary) do
      pack_string(binary)
    else
      pack_bin(binary)
    end
  end
  defp do_pack(binary, _) when is_binary(binary), do: pack_raw(binary)
  defp do_pack(list, options) when is_list(list) do
    if map?(list) do
      pack_map(list, options)
    else
      pack_array(list, options)
    end
  end
  defp do_pack(term, %{ext_packer: packer}) when is_function(packer) do
    case pack_ext(term, packer) do
      { :ok, packed } -> packed
      { :error, _ } = error -> error
    end
  end
  defp do_pack(term, _), do: { :error, { :badarg, term } }

  defp pack_int(i) when i >= -32,                  do: << 0b111 :: 3, i :: 5 >>
  defp pack_int(i) when i >= -128,                 do: << 0xD0  :: 8, i :: 8-big-signed-integer-unit(1) >>
  defp pack_int(i) when i >= -0x8000,              do: << 0xD1  :: 8, i :: 16-big-signed-integer-unit(1) >>
  defp pack_int(i) when i >= -0x80000000,          do: << 0xD2  :: 8, i :: 32-big-signed-integer-unit(1) >>
  defp pack_int(i) when i >= -0x8000000000000000 , do: << 0xD3  :: 8, i :: 64-big-signed-integer-unit(1) >>
  defp pack_int(i), do: { :error, { :too_big, i } }

  defp pack_uint(i) when i < 0x80,                do: << 0    :: 1, i :: 7 >>
  defp pack_uint(i) when i < 0x100,               do: << 0xCC :: 8, i :: 8 >>
  defp pack_uint(i) when i < 0x10000,             do: << 0xCD :: 8, i :: 16-big-unsigned-integer-unit(1) >>
  defp pack_uint(i) when i < 0x100000000,         do: << 0xCE :: 8, i :: 32-big-unsigned-integer-unit(1) >>
  defp pack_uint(i) when i < 0x10000000000000000, do: << 0xCF :: 8, i :: 64-big-unsigned-integer-unit(1) >>
  defp pack_uint(i), do: { :error, { :too_big, i } }

  # for old row format
  defp pack_raw(binary) when byte_size(binary) < 32 do
    << 0b101 :: 3, byte_size(binary) :: 5, binary :: binary >>
  end
  defp pack_raw(binary) when byte_size(binary) < 0x10000 do
    << 0xDA  :: 8, byte_size(binary) :: 16-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_raw(binary) when byte_size(binary) < 0x100000000 do
    << 0xDB  :: 8, byte_size(binary) :: 32-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_raw(binary), do: { :error, { :too_big, binary } }

  # for string format
  defp pack_string(binary) when byte_size(binary) < 32 do
    << 0b101 :: 3, byte_size(binary) :: 5, binary :: binary >>
  end
  defp pack_string(binary) when byte_size(binary) < 0x100 do
    << 0xD9  :: 8, byte_size(binary) :: 8-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_string(binary) when byte_size(binary) < 0x10000 do
    << 0xDA  :: 8, byte_size(binary) :: 16-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_string(binary) when byte_size(binary) < 0x100000000 do
    << 0xDB  :: 8, byte_size(binary) :: 32-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_string(binary), do: { :error, { :too_big, binary } }

  # for binary format
  defp pack_bin(binary) when byte_size(binary) < 0x100 do
    << 0xC4  :: 8, byte_size(binary) :: 8-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_bin(binary) when byte_size(binary) < 0x10000 do
    << 0xC5  :: 8, byte_size(binary) :: 16-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_bin(binary) when byte_size(binary) < 0x100000000 do
    << 0xC6  :: 8, byte_size(binary) :: 32-big-unsigned-integer-unit(1), binary :: binary >>
  end
  defp pack_bin(binary) do
    { :error, { :too_big, binary } }
  end

  defp pack_map([{}], options), do: pack_map([], options)
  defp pack_map(map, options) do
    case do_pack_map(map, options) do
      { :ok, binary } ->
        case length(map) do
          len when len < 16 ->
            << 0b1000 :: 4, len :: 4-integer-unit(1), binary :: binary >>
          len when len < 0x10000 ->
            << 0xDE :: 8, len :: 16-big-unsigned-integer-unit(1), binary :: binary>>
          len when len < 0x100000000 ->
            << 0xDF :: 8, len :: 32-big-unsigned-integer-unit(1), binary :: binary>>
          _ ->
            { :error, { :too_big, map } }
        end
      error ->
        error
    end
  end

  defp pack_array(list, options) do
    case do_pack_array(list, options) do
      { :ok, binary } ->
        case length(list) do
          len when len < 16 ->
            << 0b1001 :: 4, len :: 4-integer-unit(1), binary :: binary >>
          len when len < 0x10000 ->
            << 0xDC :: 8, len :: 16-big-unsigned-integer-unit(1), binary :: binary >>
          len when len < 0x100000000 ->
            << 0xDD :: 8, len :: 32-big-unsigned-integer-unit(1), binary :: binary >>
          _ ->
            { :error, { :too_big, list } }
        end
      error ->
        error
    end
  end

  def do_pack_map(map, options) do
    do_pack_map(:lists.reverse(map), <<>>, options)
  end

  defp do_pack_map([], acc, _), do: { :ok, acc }
  defp do_pack_map([{ k, v }|t], acc, options) do
    case do_pack(k, options) do
      { :error, _ } = error ->
        error
      k ->
        case do_pack(v, options) do
          { :error, _ } = error ->
            error
          v ->
            do_pack_map(t, << k :: binary, v :: binary, acc :: binary >>, options)
        end
    end
  end

  defp do_pack_array(list, options) do
    do_pack_array(:lists.reverse(list), <<>>, options)
  end

  defp do_pack_array([], acc, _), do: { :ok, acc }
  defp do_pack_array([h|t], acc, options) do
    case do_pack(h, options) do
      { :error, _ } = error ->
        error
      binary ->
        do_pack_array(t, << binary :: binary, acc :: binary >>, options)
    end
  end

  defp map?([]), do: false
  defp map?([{}]), do: true
  defp map?(list) when is_list(list), do: :lists.all(&(match?({_, _}, &1)), list)
  defp map?(_), do: false

  defp pack_ext(term, packer) do
    case packer.(term) do
      { :ok, { type, data } } when 0 < type and type < 0x100 and is_binary(data) ->
        maybe_bin = case byte_size(data) do
                      1 -> << 0xD4, type :: 8, data :: binary >>
                      2 -> << 0xD5, type :: 8, data :: binary >>
                      4 -> << 0xD6, type :: 8, data :: binary >>
                      8 -> << 0xD7, type :: 8, data :: binary >>
                      16 -> << 0xD8, type :: 8, data :: binary >>
                      size when size < 0x100 ->
                        << 0xC7, size :: 8, type :: 8, data :: binary >>
                      size when size < 0x10000 ->
                        << 0xC8, size :: 16, type :: 8, data :: binary >>
                      size when size < 0x100000000 ->
                        << 0xC9, size :: 32, type :: 8, data :: binary >>
                      _ ->
                        { :error, { :too_big, data } }
                    end
        case maybe_bin do
          { :error, _ } = error ->
            error
          bin ->
            { :ok, bin }
        end
      { :ok, other } ->
        { :error, { :invalid_ext_data, other } }
      { :error, _ } = error ->
        error
    end
  end
end
