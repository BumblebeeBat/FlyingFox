defmodule BlockAbsorber do
  def looper do
    receive do
      {:ping, p} ->
        send(p, {:ok, :pong})
        looper
      {:block, b} ->
        Blockchain.absorb(b)
        looper
      true ->
        looper
    end
  end
  def port do 5556 end
  def key do :absorber end
  def start do
    {:ok, pid} = Task.start_link(fn->looper end)
    Process.register(pid, key)
  end
  def talk(s) do send(key, s) end
end
