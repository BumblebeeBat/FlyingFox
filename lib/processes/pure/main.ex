defmodule Main do
  use Supervisor
	@name __MODULE__
	def start(x \\ 0) do start_link(x) end
  def start_link(arg) do
		Supervisor.start_link(__MODULE__, arg, name: @name)
  end
  def init(arg) do
    p=arg+Constants.tcp_port
		b=p+Constants.port_d
    children = [worker(Port, [b, p]),
								worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),
                worker(Listener, []),
                worker(InternalListener, []),
                #supervisor(Tcp, [:external, &(Listener.export(&1))], id: :external),
                #supervisor(Tcp, [:internal, &(InternalListener.export(&1))], id: :internal),
                supervisor(NewTcp, [:tcp, &(Listener.export(&1)), &(InternalListener.export(&1))]),
                #supervisor(NewTcp, [:new_internal, &(InternalListener.export(&1)), {122,0,0,1}], id: :n_internal),
                worker(Talker, []),
               ]
    supervise(children, strategy: :rest_for_one)
  end
end
