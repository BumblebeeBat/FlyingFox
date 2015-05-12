defmodule MessagePack do
  defdelegate pack(term), to: MessagePack.Packer
  defdelegate pack(term, options), to: MessagePack.Packer
  defdelegate pack!(term), to: MessagePack.Packer
  defdelegate pack!(term, options), to: MessagePack.Packer

  defdelegate unpack(term), to: MessagePack.Unpacker
  defdelegate unpack(term, options), to: MessagePack.Unpacker
  defdelegate unpack!(term), to: MessagePack.Unpacker
  defdelegate unpack!(term, options), to: MessagePack.Unpacker

  defdelegate unpack_once(term), to: MessagePack.Unpacker
  defdelegate unpack_once(term, options), to: MessagePack.Unpacker
  defdelegate unpack_once!(term), to: MessagePack.Unpacker
  defdelegate unpack_once!(term, options), to: MessagePack.Unpacker
end
