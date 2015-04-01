#example code for talking to the TcpRpc
defmodule Api do
  defp talk(port, msg) do Tcp.talk("localhost", port, msg) end
  defp absorber_talk(msg) do talk(BlockAbsorber.port, msg) end
  def buy_block do absorber_talk(BlockchainPure.buy_block) end
  defp mempool_talk(msg) do talk(Mempool.port, msg) end
  def txs do mempool_talk(["txs"]) end
end
