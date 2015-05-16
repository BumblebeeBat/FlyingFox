defmodule Main do
  use Supervisor
  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg)
  end
  def init(arg) do
    p=6666
    children = [worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),                 
                worker(Listener, []),
                worker(InternalListener, []),
                #supervisor(Tcp, [p, &(Listener.export(&1))], id: :tcp1),
                #supervisor(Tcp, [p+111, &(InternalListener.export(&1))], id: :tcp2),
                worker(Talker, []),
               ]
    #KV.put("port", p)
    #Peers.add_peer(%Peer{ip: "localhost", port: p})
    supervise(children, strategy: :rest_for_one)
  end

end
