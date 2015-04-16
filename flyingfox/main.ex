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
        {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)
        KV.put("port", p)
        Blockchain.genesis_state
        Keys.master
        Tcp.start(p, &(Listener.export(&1)))
        Talker.start
      true ->
        IO.puts("this port is already being used on this machine")
    end
  end
end
#keys and kv and peers and talker depend on nothing.
#mempool and block_absorber depend on keys and kv.
#listener depends on everything
#listener.talk depends on listener
