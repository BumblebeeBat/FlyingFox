defmodule Tcp do
  def open(port) do :gen_tcp.listen(port, [:binary, {:packet, 0}, {:active, false}]) end
  def close(socket) do :gen_tcp.close(socket) end
  def start(port, func) do
    {:ok, socket} = open(port)
    spawn_link(fn -> new_peer(socket, func) end)
  end
  defp connect(host, port) do
    {x, s} = :gen_tcp.connect(:erlang.binary_to_list(host), port, [{:active, false}, {:packet, 0}])
    cond do
      x==:ok -> s
      true -> "error"
    end
  end
  defp new_peer(socket, func) do
    {x, conn} = :gen_tcp.accept(socket)
    if x==:ok do
      spawn_link(fn -> :timer.sleep(100)
                       new_peer(socket, func) end)
      conn |> listen |> func.() |> ms(conn) #these threads need a timer or something to kill them, otherwise we end up having too many.
    else
      IO.puts "failed to connect #{inspect conn}" #emfile error!
      :timer.sleep(2000)
      spawn_link(fn -> new_peer(socket, func) end)
    end
    #new_peer(socket, func)		
  end
  defp ms(string, socket) do
    if is_pid(string) do
      true=false
    end
    m=PackWrap.pack(string)
    s=byte_size(m)
    a=<<s::size(32)>>
    :gen_tcp.send(socket, a <> m)
  end
  def talk(host, port, msg) do
    s=connect(host, port)
    if s=="error" do
      {:error, "peer is off"}
    else
      case ms(msg, s) do
        :ok -> {:ok, listen(s, "")}
        x -> {:error, x}
      end
    end
  end
  def ping(host, port) do
    s=connect(host, port)
    ms("ping", s)
  end
  defp to_bytes(list) do
    cond do
      is_binary(list) -> list
      list==[] -> <<>>
      true -> <<hd(list)>> <> to_bytes(tl(list))
    end
  end
  defp listen(conn, data \\ "") do
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
        <<_::size(32), data::binary>> = data
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
