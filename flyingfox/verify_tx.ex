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
    #convert some money from the spendable variety into the kind that is locked up for a long time. transforms money into wait-money.
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
      {a, h}!=acc[:wait] -> false #{amount, height}
      h>KV.get("height")+Constants.epoch -> false #wait 50 blocks
      true -> true
    end
  end
    #If a user wants to take part in the consensus process, they would use this transaction type to turn some of their wait-money into bond-money. The price for bond-money changes continuously over time, and more bond-money is printed and given to the people who participate. If you participate, then the value of your bond-money will slowly grow. If you dont participate, then the value will quickly shrink. 
    #If you purchase less than 1/10000th of all the money as a bond, then your profit margin is very risky. Small chance of big reward. Maybe mining pools can fix this.
    #There is a moving exchange rate. Bond-coins are constantly losing value.
  def bond2spend?(tx, txs) do
    #acc=KV.get(tx[:pub])    
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
  def ran_block(height) do
    txs = BlockchainPure.get_block(height)[:"txs"]
    cond do
      is_nil(txs) -> 0
      true ->
        txs=Enum.filter(txs, &(&1[:"data"][:"type"]=="reveal"))
        txs=Enum.map(txs, &(first_bits(&1[:"data"][:"secret"], length(&1[:"data"][:"winners"]))))
        txs
    end
  end
  def rng do
    h=KV.get("height")
    Enum.map(h-Constants.epoch..h, fn(x) ->
      cond do
        x<0 -> 0
        true -> ran_block(x)
      end 
    end)
  end
  def sign?(tx, txs) do
    #require hash(enropy+salt)
    #limit 1 sign per block
    #IO.puts("check sign tx")
    acc = KV.get(tx[:pub])
    prev = KV.get("height") 
    tot_bonds = KV.get("tot_bonds")
    prev = KV.get(to_string(prev))
    ran=rng
    l=Enum.map(tx[:data][:winners], fn(x)->winner?(acc[:bond], tot_bonds, ran, tx[:pub], x) end)
    l1=l
    l=Enum.reduce(l, true, fn(x, y) -> x and y end)
    m = length(Enum.filter(txs, fn(t)-> t[:pub]==tx[:pub] and t[:data][:type] == "sign" end))
    cond do
      acc[:bond] < Constants.minbond -> 
        IO.puts("not enough bond")
        false
      not is_binary(tx[:data][:secret_hash]) -> 
        IO.puts("should have been binary")
        false
      not l -> 
        IO.puts("not l")
        IO.puts("l1 #{inspect l1}")
        false
      length(tx[:data][:winners])<1 -> false
      m != 0 -> false#already havethis tx
      tx[:data][:prev_hash]!=prev[:data][:hash] -> 
        IO.puts("hash not match")#{inspect tx[:data][:prev_hash]} #{inspect prev[:data][:hash]}")
        false
      true -> true
    end
    #Includes hash(entropy_bit+salt).
    #includes hash of previous block.
    #~100 bond-holders are selected every block. A block requires at least 67 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 3x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
  end
  def slasher?(tx, txs) do
    {pub, _, tx}=tx
    false
    #If you can prove that the same address signed on 2 different blocks at the same height, then you can take 1/3rd of the deposit, and destroy the rest.
    #make sure they cannot do this repeatedly
  end
  def signed_block(tx) do KV.get(to_string(tx[:data][:signed_on]))[:data] end
  def sign_tx(block, tx) do block[:txs] |> Enum.filter(&(&1[:pub]==tx[:pub]))  |> Enum.filter(&(&1[:data][:type]=="sign")) end
  def wins(block, tx) do 
  b=BlockchainPure.txs_filter(block[:txs], "sign") |> Enum.filter(&(tx[:pub]==&1[:pub])) 
  length(hd(b)[:data][:winners]) 
  end
  def reveal?(tx, txs) do
    old_block=signed_block(tx)
    signed=sign_tx(old_block, tx)
    bond_size = old_block[:bond_size]
    #winners = wins(old_block, tx)
    cond do
      #make sure old_block hasn't been revealed by this person before
      #secret_hash must match.
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
        IO.puts "4"
        false
      KV.get("height")-Constants.epoch>tx[:data][:signed_on] -> 
        IO.puts "5"
        false
      true -> true
    end
    #After you sign, you wait a while, and eventually are able to make this tx. This tx reveals the random entropy_bit and salt from the sign tx, and it reclaims the safety deposit given in the sign tx. If your bit is in the minority, then your prize is bigger.
  end
  def check_tx(tx, txs) do
    cond do
      not check_logic(tx, txs) ->
        #IO.puts("bad tx for this type #{inspect tx[:data][:type]}")
        false
      not check_([tx|txs]) -> false
      true -> true
    end
  end
  def check_logic(tx, txs) do
    f=[spend: &(spend?(&1, &2)),
       spend2wait: &(spend2wait?(&1, &2)),
       wait2bond: &(wait2bond?(&1, &2)),
       bond2spend: &(bond2spend?(&1, &2)),
       sign: &(sign?(&1, &2)),
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
  def consecutive?(l) do#input is a list of numbers
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
    #after here.
    f = (fn(x) -> Enum.reduce(x, true, &(&1 and &2)) end)
    just_nonces = Enum.map(sorted_txs, fn(ts) -> 
      Enum.map(ts, &(&1[:data][:nonce])) |> Enum.sort 
    end)
    consecutive = just_nonces |> Enum.map(&(consecutive?(&1))) |> f.()
    current_nonce = Enum.map(pubs, fn(pub) -> KV.get(pub)[:nonce] end)
    starts_right = Enum.zip(current_nonce, just_nonces) |> Enum.map(fn(x)-> elem(x, 0)==hd(elem(x, 1)) end) |> f.()
     #do each person's tx nonces start on the right nonce, and then continue consecutively upward from there?
    (starts_right and consecutive) and all_have_nonce
   end
   def check_(txs) do
    spending=BlockchainPure.being_spent(txs)
    winners = txs |> BlockchainPure.txs_filter("sign") 
    winners = winners |> Enum.map(fn(t) -> t[:data][:winners]end) 
    winners = winners |> Enum.map(fn(w) -> length(w) end)
    winners = winners |> Enum.reduce(0, &(&1+&2))
     cond do
       not check_nonces(txs) -> 
         IO.puts("bad nonce")
         false
       not VerifyBalances.positive_balances(txs,spending*3/max(winners, Constants.signers_per_block*2/3))->
         IO.puts("someone spent more money than how much they have")
         false         
       true -> true
     end
  end
  def check_txs(txs) do
    cond do
      not check_logics(txs, [])  ->
        IO.puts("bad logic")
        false
      txs==[] -> 
        IO.puts("no empty blocks")
        false
      not check_(txs) -> false
      true -> true
    end
  end
  def check_logics(new, old \\ []) do
    cond do
      length(new) ==0 -> true
      check_logic(hd(new), old) -> 
        check_logics(tl(new),[hd(new)|old])
      true -> false
    end
  end
end

