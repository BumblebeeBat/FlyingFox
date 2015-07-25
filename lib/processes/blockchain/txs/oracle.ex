defmodule Oracle do
	defstruct ins: [], sigs: [], oracle_id: "", pub: ""
	def check(tx, txs) do
		#can the creator afford this?
		#no Sztorc at first.
		#what are the addresses?
		#How many addresses are needed?
		n = length(tx.data.participants)
		cond do
			n > tx.data.m -> false
			tx.data.m < 1 -> false
			true -> true
		end

	end
	def update(tx, d) do
	end
end
