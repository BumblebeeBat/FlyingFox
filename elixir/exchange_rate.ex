defmodule ExchangeRate do
  def main(n \\ 0) do#how many bonds is a cash worth?
    b=KV.get("height")-n
    :math.pow(1.001, KV.get("height"))
  end
end
