defmodule Main do
  def start(n \\ 0) do
    p=n+Listener.port
    {x, socket} = Tcp.open(p)
    cond do
      x==:ok ->
        Tcp.close(socket)
        [Keys, KV, Mempool, BlockAbsorber, Peers, Listener] |> Enum.map(fn(x) -> supervisor(fn() -> x.start end) end)
        KV.put("port", p)
        pids = [:address, :kv, :txs, :absorber, :peers, :listen]
        |> Enum.map(&(Process.whereis(&1)))
        pids = pids ++ [supervisor(fn() -> Tcp.start(p, &(Listener.export(&1)))), supervisor(Talker.start)]
        Peers.add_peer([port: p, ip: "localhost"])# :inet.getifaddrs #"wlan0" the first "addr"
      true ->
        IO.puts("this port is already being used on this machine")
    end
  end
end

#
#keys and kv and peers and talker depend on nothing.
#mempool and block_absorber depend on keys and kv.
#listener depends on everything
#listener.talk depends on listener
