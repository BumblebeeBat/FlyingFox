defmodule Api do
  defp talk(port, msg) do Tcp.talk("localhost", port, msg) end
  defp listener_talk(msg) do talk(Listener.port, msg) end
  def add_block(block) do listener_talk(["add_block", block]) end
  def txs do listener_talk(["txs"]) end
  def pushtx(tx) do listener_talk(["pushtx", tx]) end
  def blocks(start, finish) do listener_talk(["blocks", start, finish]) end
  def add_peer(peer) do listener_talk(["add_peer", peer]) end
  def all_peers do listener_talk(["all_peers"]) end
end
