defmodule FlyingFox do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec #, warn: false
    p=6666
    IO.puts("1")
    children = [worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),                 
                worker(Listener, []),
                worker(InternalListener, []),
                #supervisor(Tcp, [p, &(Listener.export(&1))], id: :tcp1), #this needs to be turned on.
                supervisor(Tcp, [p+111, &(InternalListener.export(&1))], id: :tcp2),
                worker(Talker, []),
               ]
    IO.puts("2")
    {:ok, pid1} = Supervisor.start_link(children, strategy: :rest_for_one)
    KV.put("port", p)
    Peers.add_peer(%Peer{ip: "localhost", port: p})
    IO.puts("3")
    {:ok, pid1}
  end
  def stop(_state) do :ok end
  def main(args) do
    if args == [] do args=["aa"] end
    case hd(args) do
      "start" -> start(1, 2)
      "buy_block" -> BlockAbsorber.buy_blocks(1)
      "spend" -> TxCreator.spend(hd(tl(args)), hd(tl(tl(args))))
      "block" -> 1#Blockchain.get_block(hd(tl(args)))
      "txs" -> Mempool.txs
      x -> IO.puts("undefined #{inspect x}")
    end
  end
end

"""
defmodule FlyingFox do
  import Supervisor.Spec
  def start(n \\ 0) do
    p=n+Constants.port
    {x, socket} = Tcp.open(p)
    cond do
      x==:ok ->
        Tcp.close(socket)
        children = [ worker(KV, []),
                     worker(Keys, []),
                     worker(Mempool, []), 
                     worker(BlockAbsorber, []),                 
                     worker(Peers, []),                 
                     worker(Listener, []),
                     worker(InternalListener, []) ]
        {:ok, pid1} = Supervisor.start_link(children, strategy: :rest_for_one)
        pid2 = Tcp.start(p, &(Listener.export(&1)))
        pid3 = Tcp.start(p+1, &(InternalListener.export(&1, self())))
        {:ok, pid4} = Supervisor.start_link([worker(Talker, [])], strategy: :one_for_one)
        KV.put("port", p)
        Peers.add_peer(%Peer{ip: "localhost", port: p})
        spawn_link(fn() -> killer([pid1, pid2, pid3, pid4, self()]) end)
      true ->
        IO.puts("this port is already being used on this machine. wait ~60 seconds and try again.")
        Tcp.close(socket)
    end
  end
  def killer(l) do
    receive do
      :kill -> 
        IO.puts("main kill")
        Enum.map(l, &(Process.exit(&1, :kill)))
      true -> killer(l)
    end
  end
end

"""
