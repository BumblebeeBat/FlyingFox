defmodule InternalListener do
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def export(l) do    GenServer.call(@name, {hd(l), tl(l)}) end
  def handle_call({type, args}, _from, _) do 
    spawn(fn() ->     GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
  def buy_block do
    BlockAbsorber.buy_block
    TxCreator.sign
    TxCreator.reveal
  end
  def main(type, args) do
		IO.puts("internal listener #{inspect type}")
    case type do
      "buy_block" -> buy_block
      #"buy_blocks" -> Enum.map(1..hd(args), fn(_)-> buy_block
      #                                              :timer.sleep(1000) end)
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "stop" -> 
        IO.puts("stopping args")
				FlyingFox.stop([])
      x ->
        IO.puts("is not a command #{inspect x}")
    end
  end
end
