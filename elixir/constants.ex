#these constants are the same for every node on the chain.
defmodule Constants do
  def minbond do 2_000_000_000 end
  def signers_per_block do 100 end
  def empty_account do [amount: 0, bond: 0, wait: {0, 0}] end #wait={amount, height}
  def epoch do 100 end#amount of time until reveal
  def chances_per_address do 200 end
end
