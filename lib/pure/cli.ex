defmodule Cli do
  defp lh do "localhost" end
  defp lp do Port.port end
	defp me do %Peer{port: lp, ip: lh} end
	def talk(msg, peer) do Tcp.get(peer.ip, peer.port, msg) end
	def local_talk(msg, peer) do Tcp.get_local(peer.ip, peer.port+1000, msg) end
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
		flip(talk([:blocks, start, finish], peer))
	end
	def download_blocks(n, i, peer \\ me, out \\ []) do#grabs all blocks in the range.
    lo = length(out)
    cond do
      lo >= n -> out
      true -> download_blocks(n - 1, i, peer, out ++ blocks(i+lo, i+n, peer))
    end		
	end
  def add_peer(peer, pr \\ me) do talk([:add_peer, peer], pr) end
  def all_peers(peer \\ me) do talk([:all_peers], peer) end
  def status(peer \\ me) do talk([:status], peer) end
  def buy_block(peer \\ me) do
		out = local_talk([:buy_block], peer)
		cleanup
		out
	end
	def cleanup do
		TxCreator.sign
		TxCreator.reveal
	end
	def buy_blocks_helper(n, peer \\ me) do
		1..n |> Enum.map(fn(_) ->
			local_talk([:buy_block], peer)
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
		local_talk([:spend, amount, to], peer) end
  def stop(peer \\ me) do
  talk([:stop], peer) end
end
