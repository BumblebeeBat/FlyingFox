defmodule InternalListener do
  use GenServer
  @name __MODULE__
  def init(args) do {:ok, args} end
  def start_link() do GenServer.start_link(__MODULE__, :ok, [name: @name]) end
  def export(l) do GenServer.call(@name, {hd(l), tl(l)}) end
  def handle_call({type, args}, _from, _) do 
    spawn(fn() -> GenServer.reply(_from, main(type, args)) end)
    {:noreply, []}
  end
  def main(type, args) do
    case type do
      "buy_block" -> BlockAbsorber.buy_block
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "close_channel_fast" -> TxCreator.close_channel_fast(hd(args), hd(tl(args)), hd(tl(tl(args))))
      "close_channel_timeout" -> TxCreator.close_channel_timeout(hd(args))
      "channel_spend" -> ChannelManager.spend(hd(args), hd(tl(args)))
      "sign" -> Keys.sign(hd(args))
      "stop" -> 
        IO.puts("stopping args")
				FlyingFox.stop([])
			"cleanup" ->
				TxCreator.sign
				TxCreator.reveal
      x ->
        IO.puts("is not a command #{inspect x}")
    end
  end
end
