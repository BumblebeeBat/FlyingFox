defmodule Cli do
  defp lh do "localhost" end
  defp lp do Port.external end
	defp me do %Peer{port: lp, ip: lh} end
	def talk(msg, peer) do NewTcp.get("localhost", 8000, msg) end
	#defp talk(msg, peer) do #port, ip) do
	#	port = peer.port
	#	ip = peer.ip
  #  case Tcp.talk(ip, port, msg) do
  #    {:ok, x} -> x
  #    {:error, x} -> [error: x, port: port, ip: ip]
  #  end
  #end
	def sudo_talk(msg, peer) do NewTcp.get_local("localhost", 8001, msg) end
	#defp sudo_talk(msg, peer) do
	#	talk(msg, %{peer | port: Port.internal})
	#end
  def add_block(block, peer \\ me) do
		talk([:add_block, block], peer) end
  def txs(peer \\ me) do
		talk([:txs], peer) end
  def pushtx(tx, peer \\ me) do
		talk([:pushtx, tx], peer) end
	def flip(l, out \\ []) do
		cond do
			l == [] -> out
			is_list(l) -> flip(tl(l), [hd(l)|out])
			true -> IO.puts("error #{inspect l}")
							[]
		end
	end
  def blocks(start, finish, peer \\ me) do #might not grab all blocks in he range.
		flip(talk([:blocks, start, finish], peer)) end
	
	def download_blocks(n, i, peer \\ me, out \\ []) do#grabs all blocks in the range.
		#IO.puts("download blocks #{inspect n} #{inspect i}")
		#IO.puts("out #{inspect out}")
    lo = length(out)#out should be :ok
    cond do
      lo >= n -> out
      true -> download_blocks(n - 1, i, peer, out ++ blocks(i+lo, i+n, peer))
    end		
	end
  def add_peer(peer, pr \\ me) do
		talk([:add_peer, peer], pr) end
  def all_peers(peer \\ me) do
		talk([:all_peers], peer) end
  def status(peer \\ me) do
		talk([:status], peer) end
  def buy_block(peer \\ me) do
		out = sudo_talk([:buy_block], peer)
		cleanup
		out
	end
	def cleanup do
		TxCreator.sign
		TxCreator.reveal
	end
	def buy_blocks_helper(n, peer \\ me) do
		1..n |> Enum.map(fn(_) ->
			sudo_talk([:buy_block], peer)
			cleanup
			:timer.sleep(1000)
		end)
	end
  def buy_blocks(n, peer \\ me) do
		spawn(fn -> buy_blocks_helper(n, peer) end)
	end
  def spend(amount, to, peer \\ me) do
		if is_binary(amount) do
			amount = String.to_integer(amount)
		end
		sudo_talk([:spend, amount, to], peer) end
  def stop(peer \\ me) do
  talk([:stop], peer) end
end
