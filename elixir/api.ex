#example code for talking to the TcpRpc
defmodule Api do
  defp talk(port, msg) do Tcp.talk("localhost", port, msg) end
  defp absorber_talk(msg) do talk(BlockAbsorber.port, msg) end
  def buy_block do absorber_talk(BlockchainPure.buy_block) end
  defp mempool_talk(msg) do talk(Mempool.port, msg) end
  def txs do mempool_talk(["txs"]) end
  def pushtx(tx) do mempool_talk(["add_tx", tx]) end
	def get(x) do talk(KV.port, x) end

  def listener_talk(msg) do talk(Listener.port, msg) end
  def add_block(block) do listener_talk(["add_block", block, self()]) end
  def txs do listener_talk(["txs", self()]) end
end
