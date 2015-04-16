defmodule BlockAbsorber do
  def looper do
    receive do
      {:ping, p} ->
        send(p, [:ok])
      ["buy_blocks", b] ->
        Enum.map(b, fn(x) -> 
          Blockchain.buy_block(x) end)
      ["blocks", b] ->
        Enum.map(b, fn(x) -> Blockchain.add_block(x) end)
      _ ->
        IO.puts("block absorber fail")
    end
    looper
  end
  def key do :absorber end
  def start do
    Blockchain.genesis_state              
    {:ok, pid} = Task.start_link(fn->looper end)
    Process.register(pid, key)
    pid
  end
  def ping do 
    send(key, {:ping, self()})
    receive do
      [:ok] -> {:ok, :pong}
    end    
  end
  #def absorb(block) do talk(["block", block, self()]) end
  def poly_absorb(blocks) do
    send(key, ["blocks", blocks])
  end
  def buy_block do 
    Blockchain.sign_reveal
    send(key, ["buy_blocks", [BlockchainPure.buy_block]]) end
  #Blockchain.sign_reveal
  #poly_absorb([BlockchainPure.buy_block]) end
  def buy_blocks(n) do
    Enum.map(1..n, fn(_) -> 
      :timer.sleep(1000)
      buy_block end)
  end
  def test do
    Main.start
    buy_block
  end
end
