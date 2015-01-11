defmodule Mempool do
  def start do
    {:ok, pid}=Task.start_link(fn -> looper([]) end)
    Process.register(pid, :txs)
    :ok
  end
  def looper(mem) do
    receive do
      {:dump} ->
        looper([])
      {:add_tx, tx, s} ->
        send(s, :ok)
        looper([tx|mem])
      {:txs, s} ->
        send(s, {:ok, mem})
        looper(mem)
    end
  end
  def txs do
    send(:txs, {:txs, self()})
    receive do
      {:ok, mem} -> mem
    end
  end
  def add_tx(tx) do
    cond do
      VerifyTx.check_tx(tx, txs) -> send(:txs, {:add_tx, tx, self()})
      true -> "bad tx"
    end
  end
  def dump do
    send(:txs, {:dump})
  end
  def test do
    :ok=start
    {pub, priv}=Sign.new_key
    tx=[type: :spend]
    tx=Sign.sign_tx(tx, pub, priv)
    add_tx(tx)
    IO.puts inspect txs
    dump
    IO.puts inspect txs		
  end
end
