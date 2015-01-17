defmodule Accounts do
  def empty() do
    [amount: 0, bond: 0, wait: {0, 0}]#wait={amount, height}
  end
  def balance(a) do
    acc=KV.get(a)
    Dict.get(acc, :amount)
  end
end
