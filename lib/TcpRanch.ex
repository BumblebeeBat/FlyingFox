defmodule TcpRanch do
  #use ranch_protocol
  def start_link(ref, socket, transport, opts) do
    pid = spawn_link(fn() -> init(ref, socket, transport, hd(opts)) end)
    {:ok, pid}
  end
  def init(ref, socket, transport, f) do
		IO.puts("tcpranch")
		IO.puts(inspect :ranch.accept_ack(ref))
    :ok = :ranch.accept_ack(ref)
    loop(socket, transport, f)
  end
  def loop(socket, transport, func) do
		IO.puts("tcpranch loop")
    case transport.recv(socket, 0, 5000) do
      {:ok, data} -> 
        transport.send(socket, func.(data))
        loop(socket, transport, func)
      _ -> :ok = transport.close(socket)
    end
  end
end
