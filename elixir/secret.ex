defmodule Secret do
  def encode(secret) do
    << a :: 32, b :: 32, c :: 32 >> = :crypto.rand_bytes(12)
    :random.seed(a,b,c)
    a=elem(:random.seed, 1)
    [[secret+a, 1],[secret+(2*a), 2],[secret+(3*a), 3]]
  end
  def mul_all(v) do mul_all(v, 1) end
  def mul_all(v, acc) do
    cond do
      is_tuple(v) ->mul_all(Tuple.to_list(v), acc)
      v==[] -> acc
      true -> mul_all(tl(v), acc*hd(v))
    end
  end
  def diag_mul(m, c, d) do
    #multiplies the values on a diagonal of the matrix. d is -1 or 1 for direction either down or up. c tells which column to start on.
    s=tuple_size(m)
    v=Enum.map(0..tuple_size(m)-1, fn(a) -> 
      b=c+(a*d)
      b=rem(b+(100*s), s)
      elem(elem(m, b), a)
    end)
    mul_all(List.to_tuple(v))
  end
  def souths(m) do poles(m, tuple_size(m), 0, 1) end
  def norths(m) do poles(m, tuple_size(m), 0, -1) end
  def poles(m, c, a, d) do
    #IO.puts "souths #{inspect m}"
    s=tuple_size(m)
    cond do
      c==0 -> a
      true -> poles(m, c-1, a+diag_mul(m, c, d), d)
    end
  end
  def determinant(m) do
    #in math class we called this the "basket weaving method"
    if not is_tuple(m) do
      m=to_tuple(m)
    end
    s=tuple_size(m)
    t=tuple_size(elem(m, 1))
    t=s#must be square matrix
    souths(m)-norths(m)
  end
  def to_tuple(m) do
    cond do
      is_list(m) -> List.to_tuple(Enum.map(m, fn(x) -> to_tuple(x) end))
      true -> m
    end
  end
  def decode(points, 3) do
    #this is using Cramer's rule http://2000clicks.com/mathhelp/MatrixCramersRule.aspx
    #translate our list of 3 points into 3 equations with 3 unknowns.
    eqs = Enum.map(points, fn(p)-> fn(a) ->([1, a, a*a]) end.(hd(tl(p))) end)
    answers = List.to_tuple(Enum.map(points, &(hd(&1))))
    f = fn (n) ->
      fn (m) ->
        eq=hd(Enum.slice(eqs, m, 1))
        Enum.slice(eq, 0, n-1) ++ 
          [elem(answers, m)] ++
        Enum.slice(eq, n, length(eqs))
      end
    end
    x_mat=Enum.map(0..length(eqs)-1, f.(1))#replace the values in the first column with the answers.
    determinant(x_mat)/determinant(eqs)
  end  
  def loop(mem) do
    receive do
      {:peers, p} -> 
        loop([{:peers, p}|mem])
      {:var, k, v} ->
        loop([{k, v}|mem])
      {:mem, s} ->
        send(s, {:ok, mem})
        loop(mem)
      {:mul, a, b, c} ->
        #IO.puts inspect mem
        IO.puts "peer calculates C=#{inspect hd(mem[a])*hd(mem[b])}"
        n=encode(hd(mem[a])*hd(mem[b]))
        IO.puts "peer generates polynomial for c #{inspect n}"
        send(hd(mem[:peers]), {:ok, hd(hd(n)), mem[:id]})
        send(hd(tl(mem[:peers])), {:ok, hd(hd(tl(n))), mem[:id]})
        send(hd(tl(tl(mem[:peers]))), {:ok, hd(hd(tl(tl(n)))), mem[:id]})
        new=[]
        receive do
          {:ok, p, id} -> new=[[p, id]|new]
        end
        receive do
          {:ok, p, id} -> new=[[p, id]|new]
        end
        receive do
          {:ok, p, id} -> new=[[p, id]|new]
        end
        IO.puts "after trade shares #{inspect new}"
        loop([{c, [decode(new, 3), mem[:id]]}|mem])
    end
  end
  def test do 
    IO.puts "encode #{inspect encode(5)}"
    IO.puts "decode test #{inspect decode([[2,1],[4,2],[10,3]], 3)}"
    IO.puts "decode test #{inspect decode(encode(5), 3)}"
  end
  def test2 do
    a=encode(200)
    b=encode(101)
    IO.puts "secret a #{inspect a}"
    IO.puts "secret b #{inspect b}"
    {:ok, loop1} = Task.start_link(fn -> loop([id: 1]) end)
    {:ok, loop2} = Task.start_link(fn -> loop([id: 2]) end)
    {:ok, loop3} = Task.start_link(fn -> loop([id: 3]) end)
    peers=[loop1, loop2, loop3]
    send(loop1, {:peers, peers})
    send(loop2, {:peers, peers})
    send(loop3, {:peers, peers})  
    send(loop1, {:var, :a, hd(a)})
    send(loop2, {:var, :a, hd(tl(a))})
    send(loop3, {:var, :a, hd(tl(tl(a)))})
    send(loop1, {:var, :b, hd(b)})
    send(loop2, {:var, :b, hd(tl(b))})
    send(loop3, {:var, :b, hd(tl(tl(b)))})
    send(loop1, {:mul, :a, :b, :c})
    send(loop2, {:mul, :a, :b, :c})
    send(loop3, {:mul, :a, :b, :c})
    send(loop1, {:mem, self()})
    send(loop2, {:mem, self()})
    send(loop3, {:mem, self()})
    receive do
      {:ok, x} -> m=x
    end
    receive do
      {:ok, x} -> n=x
    end
    receive do
      {:ok, x} -> o=x
    end
    IO.puts inspect decode([ m[:c],n[:c],o[:c] ], 3)
  end
end
