defmodule Api do
  defp lh do "localhost" end
  defp lp do KV.get("port") end
  defp talk(msg, port, ip) do 
    case Tcp.talk(ip, port, msg) do
      {:ok, x} -> x
      {:error, x} -> [error: x, port: port, ip: ip]
    end
  end
  def add_block(block, port \\ lp, ip \\ lh) do 
  talk([:add_block, block], port, ip ) end 
  def txs(port \\ lp, ip \\ lh) do 
  talk([:txs], port, ip) end
  def pushtx(tx, port \\ lp, ip \\ lh) do 
  talk([:pushtx, tx], port, ip) end
  def blocks(start, finish, port \\ lp, ip \\ lh) do 
  talk([:blocks, start, finish], port, ip) end
  def add_peer(peer, port \\ lp, ip \\ lh) do 
  talk([:add_peer, peer], port, ip) end
  def all_peers(port \\ lp, ip \\ lh) do 
  talk([:all_peers], port, ip) end
  def status(port \\ lp, ip \\ lh) do
  talk([:status], port, ip) end

  def buy_block(port \\ lp+1, ip \\ lh) do
  talk([:buy_block], port, ip) end
  def buy_blocks(n, port \\ lp+1, ip \\ lh) do
  talk([:buy_blocks, n], port, ip) end
  def spend(amount, to, port \\ lp+1, ip \\ lh) do
  talk([:spend, String.to_integer(amount), to], port, ip) end
  def stop(port \\ lp+1, ip \\ lh) do
  talk([:stop], port, ip) end

  def help do IO.puts "Help examples
./ff start
./ff stop
./ff buy_block
./ff buy_blocks 4
./ff block 5 #loads the fifth block
./ff txs
./ff all_peers
./ff status
./ff spend amount to
./ff -h
" end
  def main(args) do
    cond do
      args == [] -> help
      true -> main2(args)
    end
  end
  def main2(args) do
    case hd(args) do
      "-h" -> help
      "--help" -> help
      "buy_block" -> buy_block(6667) |> inspect |> IO.puts
      "buy_blocks" -> buy_blocks(String.to_integer(hd(tl(args))), 6667) |> inspect |> IO.puts
      "spend" -> spend(String.to_integer(hd(tl(args))), hd(tl(tl(args))), 6667) |> inspect |> IO.puts
      "status" -> status(6666) |> inspect |> IO.puts
      "stop" -> stop(6667) |> inspect |> IO.puts
      "txs" -> txs(6666) |> inspect |> IO.puts
      "all_peers" -> all_peers(6666) |> inspect |> IO.puts
      "start" ->  Main.start(0) |> inspect |> IO.puts
      x -> IO.puts("#{inspect x} is not defined"); help
    end
  end
end
