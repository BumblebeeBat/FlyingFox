defmodule InternalListener do
  use GenServer
  def key do :internal_listen end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: key]) end
  def init(:ok) do {:ok, []} end
  def handle_cast({type, s, args}, _) do 
    spawn_send(s, (fn() -> main(type, args) end))
    {:noreply, []}
  end
  def export(l, parent) do 
    GenServer.cast(key, {hd(l), self(), tl(l)++[parent]}) 
    receive do [:ok, x] -> x end
  end
  def buy_block do
    BlockAbsorber.buy_block
    TxCreator.sign
    TxCreator.reveal
  end
  def main(type, args) do
    case type do
      "buy_block" ->  buy_block
      "buy_blocks" -> Enum.map(1..hd(args), fn(_)-> 
                                  buy_block
                                  :timer.sleep(1000) end)
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "stop" -> 
        IO.puts("stopping args")
        send(hd(args), :kill)
      x -> 
        IO.puts("is not a command #{inspect x}")
    end
  end
  def spawn_send(s, f) do
    spawn_link(fn() ->
      send s, [:ok, f.()]
    end)
  end
end
