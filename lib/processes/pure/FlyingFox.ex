defmodule FlyingFox do
  use Application
  def start(:normal, l) do Main.start(Port.round_robin) end
  def stop(_state) do :ok end
end
