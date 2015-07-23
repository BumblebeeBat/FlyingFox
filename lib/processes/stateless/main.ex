defmodule Main do
  use Supervisor
	@name __MODULE__
	def start(x \\ 0) do start_link(x) end
  def start_link(arg) do
		Supervisor.start_link(__MODULE__, arg, name: @name)
  end
  def init(arg) do
    #p=arg+Constants.tcp_port
    p = 46666
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
								worker(MailBox, []),
								worker(MailNodes, []),
								worker(Inbox, []),
								worker(CheckMail, []),
               ]
    supervise(children, strategy: :one_for_one)
  end
end
