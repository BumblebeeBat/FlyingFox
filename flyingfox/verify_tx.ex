defmodule VerifyTx do
  def spend?(tx, txs) do
    true
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
    <<c :: size(s), d :: bitstring>>=b
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
    acc = KV.get(tx[:"pub"])
    prev = KV.get("height") 
    tot_bonds = KV.get("tot_bonds")
    prev = prev-1
    prev = KV.get(prev)
    ran=rng
    l=Enum.map(tx[:"data"][:"winners"], fn(x)->winner?(acc[:bond], tot_bonds, ran, tx[:"pub"], x) end)
    l=Enum.reduce(l, true, fn(x, y) -> x and y end)
    #IO.puts "txs #{ inspect txs}"
    m = length(Enum.filter(txs, fn(t)-> t[:"data"][:"type"] == "sign" end))
    cond do
      acc[:bond] < Constants.minbond -> false
      not is_binary(tx[:"data"][:"secret_hash"]) -> false
      not l -> false
      length(tx[:"data"][:"winners"])<1 -> false
      m != 0 -> false
      tx[:"data"][:"prev_hash"]!=prev[:"hash"] -> false
      true -> true
    end
    #Includes hash(entropy_bit+salt).
    #includes hash of previous block.
    #~64 bond-holders are selected every block. A block requires at least 43 of them to sign for it to be valid. The bond-money of each signer is shrunk to pay a safety deposit. They all pay the same amount. The amount they pay is based off how much money is spent in the spend txs in this block. Total safety deposits needs to be 1.5x as big as the total amount of money spent in spend-type txs. The most they could have to pay is as much bond-money as the poorest of them has.
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
    winners = wins(old_block, tx)
    cond do
      #make sure old_block hasn't been revealed by this person before
      #secret_hash must match.
      length(signed)==0 -> 
        IO.puts "0"
        false
      byte_size(tx[:data][:secret])!=10 -> 
        IO.puts "tx #{inspect tx}"
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
    f=[spend: &(spend?(&1, &2)),
       spend2wait: &(spend2wait?(&1, &2)),
       wait2bond: &(wait2bond?(&1, &2)),
       bond2spend: &(bond2spend?(&1, &2)),
       sign: &(sign?(&1, &2)),
       slasher: &(slasher?(&1, &2)),
       reveal: &(reveal?(&1, &2))]
    default = fn(a, b) -> false end
    cond do
      not Dict.get(f, String.to_atom(tx[:data][:type]), default).(tx, txs) -> false
      not Sign.verify_tx(tx) -> false
      true -> true
    end
  end
  def check_txs(txs) do
    spending=BlockchainPure.being_spent(txs)
    winners=BlockchainPure.txs_filter(txs, "sign")
    winners=Enum.map(winners, fn(t) -> length(t[:data][:winners])end)
    winners=Enum.reduce(winners, 0, &(&1+&2))
    #IO.puts("spending #{inspect spending}")
    #IO.puts("winner #{inspect winners}")
    cond do
      txs==[] -> 
        IO.puts("no empty blocks")
        false
      winners < Constants.signers_per_block*2/3 -> 
        IO.puts("not enough signers")
        false
      not check_logic(txs, [])  ->
        IO.puts("bad logic")
        false
      not VerifyBalances.positive_balances(txs, spending*3/winners) ->
        IO.puts("someone spent more money than how much they have")
        false
      true ->
        true
    end
  end
  def check_logic(new, old \\ []) do
    cond do
      length(new) ==0 -> true
      check_tx(hd(new), old) -> 
        check_logic(tl(new),[hd(new)|old])
      true -> false
    end
  end
end
