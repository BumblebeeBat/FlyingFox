defmodule Monitor do
  def doit(pids) do
    :timer.sleep(10000)
    IO.puts("monitor")
    pids|> Enum.map(&(Process.alive?(&1)))|> Enum.map(&(IO.puts(&1)))
    doit(pids)
  end
  def start(pids) do
    IO.puts(inspect pids)
    spawn_link(fn -> doit(pids) end)
  end
end
