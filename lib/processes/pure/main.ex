defmodule Main do
  use Supervisor
	@name __MODULE__
	def start(x \\ 0) do start_link(x) end
  def start_link(arg) do
		Supervisor.start_link(__MODULE__, arg, name: @name)
  end
	def func1 do
		Keys.new
		Keys.pubkey
	end
	def func2(key) do
		TxCreator.spend(key, 1000)
		Cli.buy_block
		TxCreator.to_channel(key, 2000)
		Cli.buy_block
		TxCreator.close_channel(key, 1000, 999)
	end
  def init(arg) do
    p=arg+Constants.tcp_port
    children = [worker(Port, [p]),
								worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),
                worker(Peers, []),
								worker(ChannelManager, []),
                worker(Listener, []),
                worker(InternalListener, []),
                supervisor(Tcp, [:tcp, &(Listener.export(&1)), &(InternalListener.export(&1))]),
                worker(Talker, []),
               ]
    supervise(children, strategy: :one_for_one)
  end
end
