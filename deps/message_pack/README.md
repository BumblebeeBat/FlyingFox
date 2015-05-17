# MessagePack for Elixir

[![Build Status](https://travis-ci.org/mururu/msgpack-elixir.png?branch=master)](https://travis-ci.org/mururu/msgpack-elixir)

## Usage

```elixir
# pack
MessagePack.pack([1,2,3]) #=> { :ok, <<147,1,2,3>> }
MessagePack.pack!([1,2,3]) #=> <<147,1,2,3>>

# unpack
MessagePack.unpack(<<147,1,2,3>>) #=> { :ok, [1,2,3] }
MessagePack.unpack!(<<147,1,2,3>>) #=> [1,2,3]

# unpack_once
MessagePack.unpack_once(<<147,1,2,3,4>>) #=> {:ok, {[1, 2, 3], <<4>>}}
MessagePack.unpack_once!(<<147,1,2,3,4>>) #=> {[1, 2, 3], <<4>>}
```

## Options

* `enable_string`

Support string type. This options is `false` by default.

```elixir
iex(1)> { :ok, bin } = MessagePack.pack(<<255>>)
{:ok, <<161, 255>>}
iex(3)> MessagePack.unpack(<<161, 255>>)
{:ok, <<255>>}
iex(4)> MessagePack.unpack(<<161, 255>>, enable_string: true)
{:error, {:invalid_string, <<255>>}}
```

* `ext`

Support extention type.

See `test/message_pack_ext_test.exs`.

## License

MIT
