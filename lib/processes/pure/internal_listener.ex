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
	def pack(o, f) do
		o |> PackWrap.unpack |> f.() |> PackWrap.pack
	end
  def main(type, args) do
    case type do
      "buy_block" -> BlockAbsorber.buy_block
      "spend" -> TxCreator.spend(hd(args), hd(tl(args)))
      "close_channel_fast" -> TxCreator.close_channel_fast(hd(args))
      "close_channel_slasher" ->
				args
				|> hd
				|> pack(&(TxCreator.close_channel_slasher(&1)))
      "close_channel_timeout" -> TxCreator.close_channel_timeout(hd(args))
      "to_channel" -> TxCreator.to_channel(hd(args), hd(tl(args)))
      "channel_spend" -> ChannelManager.spend(hd(args), hd(tl(args)))
      "channel_state" -> ChannelManager.get(hd(args))
      "sign" ->
				args
				|> hd
				|> pack(&(Keys.sign(&1)))
			"cleanup" ->
				TxCreator.sign
				TxCreator.reveal
			"newkey" -> Keys.new
			"loadkey" -> Keys.load(hd(args), hd(tl(args)))
      x ->
        IO.puts("is not a command #{inspect x}")
    end
  end
end
