defmodule FlyingFox do
  use Application
  def start(:normal, l) do
    :ranch.start_listener("FlyingFox", 1, :ranch_tcp1, [port: 6666], TcpRanch, [&(Listener.export(&1))])
    :ranch.start_listener("FlyingFox", 1, :ranch_tcp2, [port: 6666], TcpRanch, [&(InternalListener.export(&1))])
    Main.start_link([])
  end
  def stop(_state) do :ok end
end
