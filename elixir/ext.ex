defmodule MessagePack.Ext do

  use Behaviour

  @type type :: non_neg_integer
 
  defcallback pack(term) :: { :ok, { type, binary } } | { :error, term }
  defcallback unpack(type, binary) :: { :ok, term } | { :error, term }

  defmodule Behaviour do
    defmacro __using__(_) do
      quote do
        @behaviour MessagePack.Ext
      end
    end
  end
end
