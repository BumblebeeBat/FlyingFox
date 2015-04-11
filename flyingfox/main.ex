defmodule Main do
  def start(n \\ 0) do
    p=n+Listener.port
    tcp_options = [:binary, {:packet, 0}, {:active, false}]
    {x, socket} = Tcp.open(p)
    cond do
      x==:ok ->
        Tcp.close(socket)
        Keys.start
        KV.start
        Mempool.start
        BlockAbsorber.start
        Peers.start
        Listener.start
        KV.put("port", p)
        Tcp.start(p, &(Listener.talk(&1)))
        Talker.start
        Peers.add_peer([port: p, ip: "localhost"])# :inet.getifaddrs #"wlan0" the first "addr"
      true ->
        IO.puts("this port is already being used on this machine")
    end
  end
end
