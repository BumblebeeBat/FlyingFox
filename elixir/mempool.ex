defmodule Mempool do
  def looper(mem) do
    receive do
      [_, "dump"] ->
        looper([])
      [s, ["add_tx", tx]] ->
        send(s, ["ok", :ok])
        looper([tx|mem])
      [s, ["txs"]] ->
        send(s, ["ok", mem])
        looper(mem)
      _ -> 
        looper(mem)
    end
  end
  def key do :txs end
  def port do 6667 end
  def start do
    {:ok, pid}=Task.start_link(fn -> looper([]) end)
    Process.register(pid, key)
    :ok
  end
  def talk(s) do 
    send(key, [self(), s])
    receive do
      ["ok", x] -> x
    end    
  end
  def txs do
    send(key, [self(), ["txs"]])
    receive do
      ["ok", mem] -> 
        mem
    end
  end
  def add_tx(tx) do
    cond do
      VerifyTx.check_tx(tx, txs) -> 
        send(key, [self(), ["add_tx", tx]])
        receive do
          ["ok", mem] -> mem
        end
      true -> "bad tx"
    end
  end
  def dump do
    send(key, [self(), :dump])
  end
  def test do
    :ok=start
    {pub, priv}=Sign.new_key
    tx=["type": "spend"]
    tx=Sign.sign_tx(tx, pub, priv)
    add_tx(tx)
    IO.puts inspect txs
    dump
    IO.puts inspect txs		
  end
end
