defmodule Tcp do
  #use Application
  use Supervisor

  def open(port) do
    :gen_tcp.listen(port, [:binary, {:packet, 0}, {:active, false}])
  end
  def close(socket) do
    :gen_tcp.close(socket)
  end
  def start_link(a, b) do start(a, b) end
  def start(_type, _args) do
    import Supervisor.Spec
    children = [
      supervisor(Task.Supervisor, [[name: Tcp.TaskSupervisor]]),
      worker(Task, [Tcp, :accept, _args])
    ]
    opts = [strategy: :one_for_one, name: TcpServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
  def accept(port, func) do
    {:ok, socket} = open(port)
    IO.puts "Accepting connections on port #{port}"
    loop_acceptor(socket, port, func)
  end
  def loop_acceptor(socket, port, func) do
    {x, conn} = :gen_tcp.accept(socket)
    cond do
      x == :ok -> Task.Supervisor.start_child(Tcp.TaskSupervisor, fn -> serve(conn, func) end)
      true ->
        IO.puts("failed to connect #{inspect conn}")
        close(socket)
        :timer.sleep(500)
        spawn(fn -> accept(port, func) end)
    loop_acceptor(socket, port, func)
    end
  end
  def serve(client, func) do
    client |> listen |> func.() |> ms(client)
  end
  defp ms(string, socket) do
    if is_pid(string) do
      true = false
    end
    m=PackWrap.pack(string)
    s=byte_size(m)
    a=<<s::size(32)>>
    :gen_tcp.send(socket, a <> m)
  end
  defp connect(host, port) do
    {x, s} = :gen_tcp.connect(:erlang.binary_to_list(host), port, [{:active, false}, {:packet, 0}])
    cond do
      x == :ok -> s
      true -> "error"
    end
  end
  def talk(host, port, msg) do
    s = connect(host, port)
    if s == "error" do
      {:error, "peer is off"}
    else
      case ms(msg, s) do
        :ok -> {:ok, listen(s, "")}
        x -> {:error, x}
      end
    end
  end
  def ping(host, port) do
    s = connect(host, port)
    ms("ping", s)
  end
  defp to_bytes(list) do#141,000 times in first 2 blocks
    cond do
      is_binary(list) -> list
      list == [] -> <<>>
      true -> <<hd(list)>> <> to_bytes(tl(list))
    end
  end
  defp listen(conn, data \\ "") do
   case :gen_tcp.recv(conn, 0) do
      {:ok, d} -> done_listening?(conn, data <> to_bytes(d))
      {:error, :closed} -> IO.puts "error"
    end
  end
  defp done(data) do
    cond do
      byte_size(data) < 4 -> false
      true ->
        <<a::size(32), b::binary>> = data
        cond do
          byte_size(b) == a -> true
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
    port = 0
    start(port, &(&1))
    IO.puts(inspect talk("localhost", @tcp_port, [a: [b: 3, d: "e"]]))
  end
end
