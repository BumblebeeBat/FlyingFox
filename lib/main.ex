defmodule Main do
  use Supervisor
	def start do start_link([]) end
  def start_link(_arg) do
    Supervisor.start_link(__MODULE__, [])
  end
  def init(_) do
    p=6666
    children = [worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),                 
                worker(Listener, []),
                worker(InternalListener, []),
                supervisor(Tcp, [{p, :tcp1}, &(Listener.export(&1))], id: :tcp1),
                supervisor(Tcp, [{p+111, :tcp2}, &(InternalListener.export(&1))], id: :tcp2),
                worker(Talker, []),
               ]
    #KV.put("port", p)
    #Peers.add_peer(%Peer{ip: "localhost", port: p})
		IO.puts("init")
    supervise(children, strategy: :rest_for_one)
  end

end
