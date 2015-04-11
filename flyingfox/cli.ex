defmodule Cli do
def buy_block(n \\ 1) do BlockAbsorber.buy_blocks(n) end
def spend(amount, to) do TxCreator.spend(amount, to) end
def start do Main.start end
def block(n) do KV.get(to_string(n)) end
def txs do Mempool.txs end
end
