defmodule BlockAbsorber do
  def looper do
    receive do
      {:ping, p} ->
        send(p, [:ok])
      ["block", b, s] ->
        cond do
          is_binary(b) ->
            IO.puts("block is not binary")
            true=false
          true ->
            Blockchain.absorb(b)
            send(s, [:ok])
        end
      _ ->
        IO.puts("block absorber fail")
    end
    looper
  end
  def port do 6665 end
  def key do :absorber end
  def start do
    {:ok, pid} = Task.start_link(fn->looper end)
    Process.register(pid, key)
  end
  def talk(s) do 
    send(key, s)
    receive do
      [:ok] -> :ok
    end    
  end
  def absorb(block) do talk(["block", block, self()]) end
  def buy_block do absorb(BlockchainPure.buy_block) end
  def ping do talk({:ping, self()}) end
  def test do
    KV.start
    Mempool.start
    Blockchain.genesis_state
    start
    buy_block
  end
end
