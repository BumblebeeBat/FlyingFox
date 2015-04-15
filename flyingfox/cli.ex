defmodule Cli do
def buy_blocks(n \\ 1) do BlockAbsorber.buy_blocks(n) end
def buy_block do buy_blocks(1) end
def spend(amount, to) do TxCreator.spend(amount, to) end
def start do Main.start end
def block(n) do KV.get(to_string(n)) end
def txs do Mempool.txs end
end
