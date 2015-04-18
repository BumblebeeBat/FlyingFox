defmodule Main do
  import Supervisor.Spec
  def start(n \\ 0) do
    p=n+Listener.port
    {x, socket} = Tcp.open(p)
    cond do
      x==:ok ->
        Tcp.close(socket)
        children = [ worker(KV, []),
                     worker(Keys, []),
                     worker(Mempool, []), 
                     worker(BlockAbsorber, []),                 
                     worker(Peers, []),                 
                     worker(Listener, []) ]
        {:ok, pid} = Supervisor.start_link(children, strategy: :rest_for_one)
        KV.put("port", p)
        Blockchain.genesis_state
        Keys.master
        Tcp.start(p, &(Listener.export(&1)))
        Talker.start
        Peers.add_peer([ip: "localhost", port: p])
      true ->
        IO.puts("this port is already being used on this machine")
    end
  end
end
