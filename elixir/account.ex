defmodule Accounts do
  def empty() do
    [amount: 0]
  end
  def balance(a) do
    acc=KV.get(a)
    Dict.get(acc, :amount)
  end
end
