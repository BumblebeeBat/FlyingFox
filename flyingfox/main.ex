defmodule Main do
  def start(n \\ 0) do
    p=n+Listener.port
    {x, socket} = Tcp.open(p)
    cond do
      x==:ok ->
        Tcp.close(socket)
        [Keys, KV, Mempool, BlockAbsorber, Peers, Listener] |> Enum.map(fn(x) -> x.start end)
        KV.put("port", p)
        pids = [:kv, :address, :txs, :absorber, :peers, :listen]
        |> Enum.map(&(Process.whereis(&1)))
        pids = pids ++ [Tcp.start(p, &(Listener.talk(&1))), Talker.start]
        Peers.add_peer([port: p, ip: "localhost"])# :inet.getifaddrs #"wlan0" the first "addr"
      true ->
        IO.puts("this port is already being used on this machine")
    end
  end
end
