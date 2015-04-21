Flying Fox~

This is a proof of stake blockchain written in elixir. It is not yet complete.

These files have state:
block_absorber.ex
mempool.ex
peers.ex
kv.ex

Everything that looks like Elixir.*.beam is a program compiled to bytecode for the erlang virtual machine.

example of starting the blockchain, creating a block, and loading the 1st block:

$ iex

iex(1)>Main.start

{:ok, #PID<0.63.0>}

iex(2)> Keys.master

iex(3)> Cli.buy_blocks(3)

iex(3)> Cli.block(2)
