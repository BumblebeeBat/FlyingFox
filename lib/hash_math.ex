defmodule HashMath do
  def abc do ["0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9, "A": 10, "B": 11, "C": 12, "D": 13, "E": 14, "F": 15] end
  def helper(h, acc) do#this runs over 1000000 times in first 2 blocks.
    cond do
      h=="" -> acc
      true -> 
        a=String.slice(h, 0..0)
        b=String.slice(h, 1,10000)
        helper(b, acc*16+abc[String.to_atom(a)])
    end
  end
  def hex2int(h) do helper(h, 0) end
  def hash2int(h) do
    {:ok, h}=Base.decode64(h)
    h=Base.encode16(h)
    hex2int(h)
  end 
  def size(i, j \\ 0, base \\ 16) do
    cond do
      :math.pow(base, j) > i -> j-1
      true -> size(i, j+1)
    end
  end
  def pow(a, b) do
    b=f2i(b)
    cond do
      b==1 -> a
      rem(b, 2)==0 -> 
        pow(a*a, f2i(b/2))
      true -> a*pow(a, b-1)
    end
  end
  def f2i(f) do 
    {i, _}= :string.to_integer(to_char_list(to_string(f)))
  f2i(f, 0, i) end
  def f2i(f, lower, upper) do
    #IO.puts "upper lower f #{inspect upper}, #{inspect lower}, #{inspect f}"
    mid=div((lower+upper), 2)
    odd=rem((lower+upper), 2)
    cond do
      lower>=upper-1 -> 
        cond do
          f>mid -> upper
          true -> lower
      end
      f>mid -> f2i(f, mid+odd, upper)
      f<mid -> f2i(f, lower, mid)
    end
  end
  def to16(i, s) do
    ten=["10": "A", "11": "B", "12": "C", "13": "D", "14": "E", "15": "F"]
    p=pow(16, s)
    n=div(i, p)
    r=rem(i, p)
    rest = fn() -> to16(r, s-1) end
    f = fn(r) -> ten[String.to_atom(to_string(r))] end
    g = fn(n) -> to_string(n) end
    cond do
      s==1 -> 
        cond do
          n>9 -> f.(r)
          true -> g.(r)
        end
      n>9 -> f.(n)<>rest.()
      true -> g.(n)<>rest.()
    end
  end
  def int2hash(i) do 
    s=to16(i, size(i))  
    {:ok, s}=Base.decode16(s)
    Base.encode64(s)
  end
  def test do
    IO.puts hex2int("100")
    IO.puts hex2int("101")
    IO.puts DetHash.doit(2)
    IO.puts hash2int(DetHash.doit(2))
    #IO.puts inspect f2i(100.0)
    #IO.puts DetHash.doit(1)
    #IO.puts int2hash(hash2int(DetHash.doit(1)))
  end
end
