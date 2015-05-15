defmodule SMPC do
  def size do 30029 end
  defp rem(x) do
    o = rem(x, size)
    if o<0 do o = o+size end
    o
  end
  def mul(x, y) do rem(x*y) end
  def add(x, y) do rem(x+y) end
end

defmodule Lib do
  def equal0(x) do #works up to size 32385
    #costs 8 multiplications.
    #http://en.wikipedia.org/wiki/Fermat%27s_little_theorem
    f = fn(x) -> SMPC.mul(x, x) end
    #x128 = Enum.reduce(1..7, x, fn(x, y) -> f.(y) end)
    x128 = f.(f.(f.(f.(f.(f.(f.(x))))))) #this number and
    x256 = f.(x128) #this number get revealed.
    rem(x256, 255) == 0 and
    rem(x128, 127) == 0
  end
  def sub(x, y) do
    SMPC.add(x, SMPC.mul(y, -1))
  end
  def equal(x, y) do
    equal0(sub(x, y))
  end
  def test(x \\ 1) do
    cond do
      x > SMPC.size - 1 -> true
      equal0(x) -> 
        IO.puts(x)
        false
      true -> test(x+1)
    end
  end
end
