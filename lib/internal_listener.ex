defmodule InternalListener do
  use GenServer
  @name __MODULE__
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: key])
  end
  def init(args) do {:ok, args} end
  def key do :internal_listen end
  def handle_call({type, args}, _from, _) do 
    spawn(fn() -> GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
  def export(l) do 
    GenServer.call(key, {hd(l), tl(l)}) 
  end
  def buy_block do
    BlockAbsorber.buy_block
    TxCreator.sign
    TxCreator.reveal
  end
  def main(type, args) do
    case type do
      "buy_block" -> buy_block
      "buy_blocks" -> Enum.map(1..hd(args), fn(_)-> buy_block
                                                    :timer.sleep(1000) end)
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "stop" -> 
        IO.puts("stopping args")
        send(hd(args), :kill)
      x ->
        IO.puts("is not a command #{inspect x}")
    end
  end
end
