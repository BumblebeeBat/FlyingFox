defmodule Tcp do
  def start(port, func) do
    tcp_options = [:binary, {:packet, 0}, {:active, false}]
    {:ok, socket} = :gen_tcp.listen(port, tcp_options)
    new_peer(socket, func)
  end
  defp connect(host, port) do
    {:ok, s} = :gen_tcp.connect(:erlang.binary_to_list(host), port, [{:active, false}, {:packet, 0}])
    s
  end
  defp new_peer(socket, func) do
    {:ok, conn} = :gen_tcp.accept(socket)
    #fun=&(&1)
    #if (type == :absorb) do fun=&(Blockchain.absorb(&1)) end
    Task.start_link(fn -> ms(conn, func.(listen(conn, ""))) end)
    new_peer(socket, type)		
  end
  defp ms(socket, string) do
    m=MessagePack.pack!(string)
    :ok = :gen_tcp.send(socket, m)
  end
  def talk(host, port, msg) do
    s=connect(host, port)
    ms(s, msg)
    listen(s, "")
  end
  def ping(host, port) do
    s=connect(host, port)
    ms(s, "ping")
  end
  defp to_bytes(list) do
    cond do
      is_binary(list) -> list
      list==[] -> <<>>
      true -> <<hd(list)>> <> to_bytes(tl(list))
    end
  end
  defp listen(conn, data) do
    case :gen_tcp.recv(conn, 0) do
      {:ok, d} ->
        done_listening?(conn, data<>to_bytes(d))
      {:error, :closed} ->
        IO.puts "error"
    end
  end
  defp done_listening?(conn, data) do
    {state, da}=MessagePack.unpack(data)
    cond do
      state == :ok -> da
      true -> listen(conn, data)
    end
  end
end
