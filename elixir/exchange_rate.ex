defmodule ExchangeRate do
  def main do#how many bonds is a cash worth?
    b=KV.get("height")
    :math.pow(1.001, KV.get("height"))
  end
end
