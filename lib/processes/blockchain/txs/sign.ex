defmodule Sign do
  defstruct nonce: 0, height: 0, secret_hash: nil, winners: [], prev_hash: nil, pub: ""
  def first_bits(b, s) do
    << c :: size(s), _ :: bitstring >> = b
    s = s + 8 - rem(s, 8) #so that we have an integer number of bytes.
    << c :: size(s) >>
  end
  def ran_block(block) do
    txs = block.data.txs
    cond do
      is_nil(txs) -> 0
      true ->
        txs |> Enum.filter(&(&1.data.__struct__=="reveal"))
        |> Enum.map(&(first_bits(&1.data.secret, length(&1.data.winners))))
        |> Enum.reduce("", &(&1 <> &2))
    end
  end
  def rng(hash, counter \\ 26, entropy \\ "" ) do 
    block = KV.get(hash)
    cond do
      block == nil -> DetHash.doit(entropy)
      counter < 1 -> DetHash.doit(entropy)
      true -> rng(block.data.hash, counter - 1, ran_block(block) <> entropy)
    end
  end
	def winner?(balance, total, seed, pub, j) do#each address gets 200 chances.
    max = HashMath.hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    b = max * Constants.signers_per_block * balance / (Constants.chances_per_address * total)
    a = HashMath.hash2int(DetHash.doit({seed, pub, j}))
    a < b and j >= 0 and j < Constants.chances_per_address and is_integer(j)
  end
	def check(tx, txs, prev_hash) do
    acc = KV.get(tx.data.pub)
		if acc == nil do
			false
		else
			check_2(tx, txs, prev_hash, acc)
		end
	end
	def check_2(tx, txs, prev_hash, acc) do
    tot_bonds = KV.get("tot_bonds")
    ran = rng(prev_hash)
    prev_block = KV.get(prev_hash)
		l = Enum.map(tx.data.winners, fn(x)->winner?(acc.bond, tot_bonds, ran, tx.data.pub, x) end)
		l = Enum.reduce(l, true, fn(x, y) -> x and y end)
    m = length(Enum.filter(txs, fn(t) -> t.data.pub == tx.data.pub and t.data.__struct__ == :Elixir.Sign end))
    height = KV.get("height")
    tx_prev = tx.data.prev_hash
    cond do
      acc.bond < Constants.min_bond ->
        IO.puts("not enough bond-money to validate")
        false
      not is_binary(tx.data.secret_hash) ->
        IO.put("should have been binary")
        false
      tx.data.height != prev_block.data.height->
        IO.puts("bad height")
        false
      not l ->
        IO.puts("not l")
        false
      length(tx.data.winners) < 1 ->
				IO.puts("too short")
				false
      m != 0 ->
				false
      not(height == 0) and tx_prev != prev_hash -> false
      true -> true
    end
	end
	def update(tx, d, bond_size) do#0.1% of total bonds is given out as rewards on every block, which changes the exchange rate.
    w = length(tx.data.winners)
    delta = -TxUpdate.exchange_rate * bond_size * w
    b = KV.get("tot_bonds")
    KV.put("tot_bonds", b + delta * d)
    TxUpdate.sym_increment(tx.data.pub, :bond, delta, d)
    #loses some :bond money. total_money
    #The most they could have to pay is as much bond-money as the poorest of them has.		
	end
end
