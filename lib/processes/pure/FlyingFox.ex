defmodule FlyingFox do
  use Application
	def ran do
		:random.seed(:erlang.now)
		trunc(Constants.max_nodes*:random.uniform())
	end
  def start(:normal, l) do Main.start(ran) end
  def stop(_state) do :ok end
end
