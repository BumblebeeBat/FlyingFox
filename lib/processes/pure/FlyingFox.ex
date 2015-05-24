defmodule FlyingFox do
  use Application
  def start(:normal, l) do Main.start end
  def stop(_state) do :ok end
end
