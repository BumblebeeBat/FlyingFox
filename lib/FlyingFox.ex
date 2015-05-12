defmodule FlyingFox do
  use Application

  #####
  # API

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [worker(KV, []),
                worker(Keys, []),
                worker(Mempool, []), 
                worker(BlockAbsorber, []),                 
                worker(Peers, []),                 
                worker(Listener, []),
                worker(InternalListener, []) ]

  end
  def stop(_state), do: :ok
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
