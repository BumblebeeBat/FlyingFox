defmodule Api do
  defp talk(port, msg) do Tcp.talk("localhost", port, msg) end
  def listener_talk(msg) do talk(Listener.port, msg) end
  def add_block(block) do listener_talk(["add_block", block]) end
  def buy_block do add_block(BlockchainPure.buy_block) end
  def txs do listener_talk(["txs"]) end
  def pushtx(tx) do listener_talk(["pushtx", tx]) end
end
