defmodule VerifyTx do
  def spend?(tx, txs) do
    cond do
      tx[:data][:fee] < Constants.min_tx_fee ->
        IO.puts("fee too low")
        false
      tx[:data][:amount]+tx[:data][:fee] > Constants.max_bond_block ->
        IO.puts("too much money at once")
        false
      true -> true
    end
  end
  def spend2wait?(tx, txs) do
    acc=KV.get(tx[:"pub"])
    cond do
      {0,0}!=acc[:wait] -> false
      true -> true
    end
  end 
  def wait2bond?(tx, txs) do
    acc=KV.get(tx[:"pub"])    
    {a, h}=tx[:"data"][:"wait_money"]
    cond do
      {a, h}!=acc[:wait] -> false 
      h>KV.get("height")+Constants.epoch -> false 
      true -> true
    end
  end
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #There is a moving exchange rate. Bond-coins are constantly losing value.
  def bond2spend?(tx, txs) do
    true
  end
  def winner?(balance, total, seed, pub, j) do#each address gets 200 chances.
    max=HashMath.hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    b=max*Constants.signers_per_block*balance/(Constants.chances_per_address*total)
    a=HashMath.hash2int(DetHash.doit({seed, pub, j}))
    a<b and j>=0 and j<Constants.chances_per_address and is_integer(j)
  end
  def first_bits(b, s) do
    <<c :: size(s), _ :: bitstring>>=b
    c
  end
  def ran_block(block) do
    #txs = BlockchainPure.get_block(height)[:"txs"]
    txs = block[:data][:txs]
    cond do
      is_nil(txs) -> 0
      true ->
        txs=Enum.filter(txs, &(&1[:"data"][:"type"]=="reveal"))
        txs=Enum.map(txs, &(first_bits(&1[:"data"][:"secret"], length(&1[:"data"][:"winners"])))) |> Enum.reduce("", &(&1 <> &2))
    end
  end
  def rng(hash, counter \\ 26, entropy \\ "" ) do #this needs to be memoized so bad.
    block = KV.get(hash)
    hash = block[:data][:hash]
    cond do
      hash == nil -> DetHash.doit(entropy)
      counter < 1 -> DetHash.doit(entropy)
      true -> rng(hash, counter - 1, ran_block(block) <> entropy)
    end
  end
  def sign?(tx, txs, prev_hash) do#block = [data: [hash: block_hash]]
    acc = KV.get(tx[:pub])
    tot_bonds = KV.get("tot_bonds")
    ran=rng(prev_hash)
    l=Enum.map(tx[:data][:winners], fn(x)->winner?(acc[:bond], tot_bonds, ran, tx[:pub], x) end)
    l1=l
    l=Enum.reduce(l, true, fn(x, y) -> x and y end)
    m = length(Enum.filter(txs, fn(t)-> t[:pub]==tx[:pub] and t[:data][:type] == "sign" end))
    cond do
      acc[:bond] < Constants.minbond -> 
        IO.puts("not enough bond-money to validate")
        false
      not is_binary(tx[:data][:secret_hash]) -> 
        IO.puts("should have been binary")
        false
      not l -> 
        IO.puts("not l")
        IO.puts("l1 #{inspect l1}")
        false
      length(tx[:data][:winners])<1 -> false
      m != 0 -> false
      not(KV.get("height")==0) and tx[:data][:prev_hash]!=prev_hash -> 
        IO.puts("hash not match")
        IO.puts("prev hash: #{inspect prev_hash}")
        IO.puts("tx: #{inspect tx}")
        false
      true -> true
    end
  end
  def slasher?(tx, txs) do
    {pub, _, tx}=tx
    false
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
    #make sure they cannot do this repeatedly
  end
  def signed_block(tx) do Blockchain.get_block(tx[:data][:signed_on]) end
  def sign_tx(block, pub) do block[:data][:txs] |> Enum.filter(&(&1[:pub]==pub))  |> Enum.filter(&(&1[:data][:type]=="sign")) end
  def wins(block, tx) do 
  b=Blockchain.txs_filter(block[:txs], "sign") |> Enum.filter(&(tx[:pub]==&1[:pub])) 
  length(hd(b)[:data][:winners]) 
  end
  def reveal?(tx, txs) do
    old_block=signed_block(tx)
    revealed = txs |> Enum.filter(&(&1[:data][:type] == "reveal")) |> Enum.filter(&(&1[:pub]==tx[:pub]))
    signed=sign_tx(old_block, tx[:pub])
    bond_size = old_block[:data][:bond_size]
    cond do
      length(revealed) > 0 -> false
      length(signed)==0 -> 
        IO.puts "0"
        false
      byte_size(tx[:data][:secret])!=10 -> 
        IO.puts "1"
        false
      DetHash.doit(tx[:data][:secret]) != hd(signed)[:data][:secret_hash] -> 
        IO.puts "2"
        false
      tx[:pub] in old_block[:meta][:revealed] ->
        IO.puts "3"
        false
      tx[:data][:amount]!=bond_size*length(tx[:data][:winners]) -> 
        IO.puts "4 slfjksd"
        false
      KV.get("height")-Constants.epoch>tx[:data][:signed_on] -> 
        IO.puts "5"
        false
      true -> true
    end
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
  def check_tx(tx, txs, prev_hash) do
    cost = Constants.block_creation_fee
    cond do
      not check_logic(tx, txs, prev_hash) -> false
      not check_([data: [txs: [tx|txs]]], cost) -> false
      true -> true
    end
  end
  def check_logic(tx, txs, prev_hash) do
    f=[spend: &(spend?(&1, &2)),
       spend2wait: &(spend2wait?(&1, &2)),
       wait2bond: &(wait2bond?(&1, &2)),
       bond2spend: &(bond2spend?(&1, &2)),
       sign: &(sign?(&1, &2, prev_hash)),
       slasher: &(slasher?(&1, &2)),
       reveal: &(reveal?(&1, &2))]
    default = fn(_, _) -> false end
    cond do
      tx[:data][:type] == nil -> false
      not Dict.get(f, String.to_atom(tx[:data][:type]), default).(tx, txs) ->  false #to_atom is DANGEROUS
      not Sign.verify_tx(tx) -> 
        IO.puts("bad signature")
        false
      true -> true
    end
  end
  def remove_repeats(l) do
    cond do
      l==[] -> l
      hd(l) in tl(l) -> remove_repeats(tl(l))
      true -> [hd(l)|remove_repeats(tl(l))]
    end
  end
  def consecutive?(l) do# l is a list of numbers
    cond do
      length(l) < 2 -> true
      hd(l)+1 == hd(tl(l)) -> consecutive?(tl(l))
      true -> false
    end
  end
  def check_nonces(txs) do
    have_nonce = Enum.map(txs, fn(tx) -> tx[:data][:nonce]!=nil end)
    all_have_nonce = Enum.reduce(have_nonce, true, &(&1 and &2))
    pubs = txs |> Enum.map(fn(tx) -> tx[:pub] end) |> remove_repeats
    sorted_txs = Enum.map(pubs, fn(pub) -> Enum.filter(txs,  &(&1[:pub]==pub)) end)
    f = (fn(x) -> Enum.reduce(x, true, &(&1 and &2)) end)
    just_nonces = Enum.map(sorted_txs, fn(ts) -> 
      Enum.map(ts, &(&1[:data][:nonce])) |> Enum.sort 
    end)
    consecutive = just_nonces |> Enum.map(&(consecutive?(&1))) |> f.()
    current_nonce = Enum.map(pubs, fn(pub) -> KV.get(pub)[:nonce] end)
    starts_right = Enum.zip(current_nonce, just_nonces) |> Enum.map(fn(x)-> elem(x, 0)==hd(elem(x, 1)) end) |> f.()
    (starts_right and consecutive) and all_have_nonce
   end
   def check_(block, cost) do
     txs = block[:data][:txs]
     spending = Blockchain.being_spent(txs)
     winners = txs 
     |> Blockchain.txs_filter("sign") 
     |> Enum.map(fn(t) -> t[:data][:winners] end) 
     |> Enum.map(fn(w) -> length(w) end)
     |> Enum.reduce(0, &(&1+&2))
     cond do
       not check_nonces(txs) -> 
         IO.puts("bad nonce")
         false
       not VerifyBalances.positive_balances(txs,spending*3/max(winners, Constants.signers_per_block*2/3), block[:pub], cost)->
         IO.puts("someone spent more money than how much they have")
         false         
       true -> true
     end
  end
  def check_txs(block, cost) do#accept block as input
    txs = block[:data][:txs]
    prev_hash = block[:data][:hash]
    cond do
      not check_logics(txs, prev_hash, [])  ->
        IO.puts("bad logic")
        false
      txs==[] -> 
        IO.puts("no empty blocks")
        false
      not check_(block, cost) -> false
      true -> true
    end
  end
  def check_logics(new, prev_hash, old \\ []) do
    cond do
      length(new) ==0 -> true
      check_logic(hd(new), old, prev_hash) -> 
        check_logics(tl(new), prev_hash, [hd(new)|old])
      true -> false
    end
  end
end

