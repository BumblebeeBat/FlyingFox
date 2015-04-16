defmodule Stack do
  use GenServer

  def start_link(state) do
    GenServer.start_link(__MODULE__, state, [name: :sup_stack])
  end

  def handle_call(:pop, _from, [h|t]) do
    {:reply, h, t}
  end

  def handle_cast({:push, h}, t) do
    {:noreply, [h|t]}
  end
end
