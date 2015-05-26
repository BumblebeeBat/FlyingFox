defmodule Cli do
  defp lh do "localhost" end
  defp lp do 6666 end
	defp talk(msg, port, ip) do
    case Tcp.talk(ip, port, msg) do
      {:ok, x} -> x
      {:error, x} -> [error: x, port: port, ip: ip]
    end
  end
	def delt do Constants.internal_d end
  def add_block(block, port \\ lp, ip \\ lh) do
		talk([:add_block, block], port, ip ) end
  def txs(port \\ lp, ip \\ lh) do
		talk([:txs], port, ip) end
  def pushtx(tx, port \\ lp, ip \\ lh) do
		talk([:pushtx, tx], port, ip) end
  def blocks(start, finish, port \\ lp, ip \\ lh) do#might not grab all blocks in he range.
		talk([:blocks, start, finish], port, ip) end
	def download_blocks(n, i, out \\ [], port \\ lp, ip \\ lh) do#grabs all blocks in the range.
    lo = length(out)
    cond do
      lo >= n -> out
      true -> download_blocks(n - 1, i, out ++ blocks(i+lo, i+n, port, ip), port, ip)
    end		
	end
  def add_peer(peer, port \\ lp, ip \\ lh) do
		talk([:add_peer, peer], port, ip) end
  def all_peers(port \\ lp, ip \\ lh) do
		talk([:all_peers], port, ip) end
  def status(port \\ lp, ip \\ lh) do
		talk([:status], port, ip) end
  def buy_block(port \\ lp, ip \\ lh) do
		#spawn(fn -> talk([:buy_block], port+delt, ip) end)
		talk([:buy_block], port+delt, ip)
		cleanup
	end
	def cleanup do
		TxCreator.sign
		TxCreator.reveal
	end
	def buy_blocks_helper(n, port, ip) do
		1..n |> Enum.map(fn(_) ->
			talk([:buy_block], port, ip)
			cleanup
			:timer.sleep(1000)
		end)
	end
  def buy_blocks(n, port \\ lp, ip \\ lh) do
		spawn(fn -> buy_blocks_helper(n, port+delt, ip) end)
	end
  def spend(amount, to, port \\ lp, ip \\ lh) do
  talk([:spend, String.to_integer(amount), to], port+delt, ip) end
  def stop(port \\ lp, ip \\ lh) do
  talk([:stop], port+delt, ip) end
end
