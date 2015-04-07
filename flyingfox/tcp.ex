defmodule Tcp do
  def start(port, func) do
    tcp_options = [:binary, {:packet, 0}, {:active, false}]
    {:ok, socket} = :gen_tcp.listen(port, tcp_options)
    Task.start_link(fn ->
      new_peer(socket, func)
    end)
  end
  defp connect(host, port) do
    {x, s} = :gen_tcp.connect(:erlang.binary_to_list(host), port, [{:active, false}, {:packet, 0}])
    cond do
      x==:ok -> s
      true -> "error"
    end
  end
  defp new_peer(socket, func) do
    {:ok, conn} = :gen_tcp.accept(socket)
    out= func.(listen(conn, ""))
    Task.start_link(fn -> ms(conn, out) end)
    new_peer(socket, func)		
  end
  defp ms(socket, string) do
    if is_pid(string) do
      true=false
    end
    m=PackWrap.pack(string)
    s=byte_size(m)
    a=<<s::size(32)>>
    :ok = :gen_tcp.send(socket, a <> m)
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
  defp done(data) do
    cond do
      byte_size(data)<4 -> false
      true ->
        <<a::size(32), b::binary>>=data
        cond do
          byte_size(b)==a -> true
          true -> false
        end
    end
  end
  defp done_listening?(conn, data) do
    cond do
      done(data) -> 
        <<a::size(32), data::binary>> = data
        PackWrap.unpack(data)
      true -> listen(conn, data)
    end
  end
  def test do
    port=6666
    start(port, &(&1))
    IO.puts(inspect talk("localhost", port, [a: [b: 3, d: "e"]]))
  end
end
