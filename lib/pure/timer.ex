defmodule Timer do
	#defstruct a: nil, b: nil, c: nil
	#def itoe({a, b, c}) do %Timer{a: a, b: b, c: c} end
	#def etoi(x) do {x.a, x.b, x.c} end
	#def stamp do :os.timestamp |> itoe end
	def prec do 1000000 end
	def stamp do
    {a, b, c} = :os.timestamp
		(((a * prec) + b) * prec) + c
	end
	def diff(x, y) do x - y end
	def now_diff(x) do diff(stamp, x) end
end
