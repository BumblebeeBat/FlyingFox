defmodule Main do
  use Supervisor
	def start(x \\ 0) do start_link(x) end
  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg)
  end
  def init(arg) do
    p=arg+Constants.tcp_port
    children = [worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),
                worker(Listener, []),
                worker(InternalListener, []),
                supervisor(Tcp, [{p, :tcp1}, &(Listener.export(&1))], id: :tcp1),
                supervisor(Tcp, [{p+Constants.internal_d, :tcp2}, &(InternalListener.export(&1))], id: :tcp2),
                worker(Talker, [p]),
               ]
    supervise(children, strategy: :rest_for_one)
  end
end
