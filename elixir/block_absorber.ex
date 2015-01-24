defmodule BlockAbsorber do
  def looper do
    receive do
      {:ping, p} ->
        send(p, {:ok, :pong})
        looper
      {:get, v, p} ->
        send(p, {:ok, KV.get(v)})
        looper
      {:block, b} ->
        Block.add_block(b)
        Block.create_sign
        Block.create_reveal
        looper
      true ->
        looper
    end
  end
  def serve do
    start
    Server.start(5555, :absorb)
  end
  def start do
    Block.start
    {:ok, pid} = Task.start_link(fn->looper end)
    Process.register(pid, :absorber)
    Block.create_sign
    Block.create_reveal
  end
  def talk(s) do
    Server.talk("localhost", 5555, s)
  end
  def test3 do
    talk(Block.buy_block)
  end
  def test2 do
    b=Block.buy_block
    IO.puts inspect b
    #send(pid, {:block, b})
    #IO.puts inspect send(pid, {:get, "1", self})    
  end
  def test do
    {:ok, pid} = Task.start_link(fn->looper end)
    send(pid, {:ping, self()})
    receive do
      {:ok, :pong} -> IO.puts "success"
    end
  end
end
