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
  talk(["add_block", block], port, ip ) end 
  def txs(port \\ lp, ip \\ lh) do 
  talk(["txs"], port, ip) end
  def pushtx(tx, port \\ lp, ip \\ lh) do 
  talk(["pushtx", tx], port, ip) end
  def blocks(start, finish, port \\ lp, ip \\ lh) do 
  talk(["blocks", start, finish], port, ip) end
  def add_peer(peer, port \\ lp, ip \\ lh) do 
  talk(["add_peer", peer], port, ip) end
  def all_peers(port \\ lp, ip \\ lh) do 
  talk(["all_peers"], port, ip) end
  def status(port \\ lp, ip \\ lh) do
    talk(["status"], port, ip) end
end
