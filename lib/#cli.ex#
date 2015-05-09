defmodule Cli do
  def buy_blocks(n \\ 1) do spawn_link(fn()->(BlockAbsorber.buy_blocks(n)) end) end
  def buy_block do buy_blocks(1) end
  def spend(amount, to) do TxCreator.spend(amount, to) end
  def start do Main.start end
  def block(n) do Blockchain.get_block(n) end
  def txs do Mempool.txs end
end
