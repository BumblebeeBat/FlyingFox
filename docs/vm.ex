#an example of the kind of VM that oracles might use.
defmodule VM do
  def out_of_gas do IO.puts "you ran out of gas" end
  def run(db) do
    cond do
      db[:gas]<0 -> out_of_gas
      length(db[:code])==0 -> db
      true ->
        op=hd(db[:code])
        cond do
          is_atom(op) ->
            lan=db[:language]
            word=lan[op]
            db=Dict.put(db, :gas, db[:gas]-fee(op))
            db=word.(db)
            db=Dict.put(db, :code, tl(db[:code]))
            run(db)
          true ->
            IO.puts "error"
            #run(db)
        end
    end
  end
  def fee(op) do 
    case op do
      _ -> 10
    end
  end
  def first_n(n, a) do
    cond do
      a==[] -> []
      n>0 -> [hd(a)|first_n(n-1, tl(a))]
      true -> []
    end               
  end
  def dup(db) do
    db = db |> Dict.put(:code, tl(db[:code])) 
    f=first_n(hd(db[:code]), db[:stack])
    db |> Dict.put(:stack, f ++ db[:stack])
  end
  def binary(db, f) do
    a=hd(db[:stack])
    b=hd(tl(db[:stack]))
    db |> Dict.put(:stack, [f.(a,b)|tl(tl(db[:stack]))])
  end
  def mul(db) do binary(db, &(&1*&2)) end
  def sum(db) do binary(db, &(&1+&2)) end
  def minus(db) do binary(db, &(&1-&2)) end
  def divide(db) do binary(db, &(div(&1, &2))) end
  def remainder(db) do binary(db, &(rem(&1, &2))) end
  def flip([], out) do out end
  def flip(a, out) do flip(tl(a), [hd(a)|out]) end
  def flip(a) do flip(a, []) end
  def split(n, a) do split_helper(n, a, []) end
  def split_helper(n, [], out) do {flip(out), []} end
  def split_helper(0, a, out) do {flip(out), a} end #flip?
  def split_helper(n, a, out) do split_helper(n-1, tl(a), [hd(a)|out]) end
  def push(db) do
    db = db |> Dict.put(:code, tl(db[:code])) 
    {data, code} = split(hd(db[:code]), tl(db[:code]))
    db |> Dict.put(:code, [27|code]) |> Dict.put(:stack, data++db[:stack])
  end
  def drop(db) do db |> Dict.put(:stack, tl(db[:stack])) end
  def swap(db) do
    db |> Dict.put(:stack, [hd(tl(db[:stack])), hd(db[:stack])]++tl(tl(db[:stack])))
  end
  def language do
    [push: &(push(&1)),
     dup: &(dup(&1)),
     *: &(mul(&1)),
     +: &(sum(&1)),
     -: &(minus(&1)),
     /: &(divide(&1)),
     %: &(remainder(&1)),
     drop: &(drop(&1)),
     swap: &(swap(&1))]
     #+: &(plus(&1))]
  end
  def empty_db(code) do [gas: 500000, language: language, stack: [], code: code, mem: <<>>] end
  def test do
    code = [:push, 2, 3, 1, :*]
    run(empty_db(code))[:stack]
  end
  def test_speed do
    code = [:push, 5, 1,2,3,4,5, :dup, 5, :*, :*, :*, :*, :*, :*, :drop, :drop, :drop, :drop] #19 opcodes
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code
    code = code ++ code #19*2^10=~19000   ##40 kb per second
    db = empty_db(code)
    IO.puts inspect :timer.tc(fn -> run(db) end, [])
    code = [:push, 5, 1,2,3,4,5, :dup, 3, :*, :*, :*, :drop, :drop, :drop, :drop, :drop] #17 opcodes
    db=empty_db(code)
    IO.puts inspect :timer.tc(fn -> run(db) end, [])
    #b=run(db)
    #IO.puts inspect b
  end
end
