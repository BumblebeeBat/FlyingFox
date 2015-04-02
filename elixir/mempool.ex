defmodule Mempool do
  def looper(mem) do
    IO.puts("mempool looper #{inspect mem}")
    receive do
      ["dump"] -> mem=[]
      [s, ["add_tx", tx]] ->
        cond do
          VerifyTx.check_tx(tx, txs) -> 
            send(s, ["ok", "ok"])
            mem=[tx|mem]
          true -> send(s, ["ok", "bad tx"])
        end
      [s, ["txs"]] ->
        send(s, ["ok", mem])
      x -> IO.puts("looper weird #{inspect x}")
    end
    looper(mem)
  end
  def key do :txs end
  def port do 6667 end
  def start do
    {:ok, pid}=Task.start_link(fn -> looper([]) end)
    Process.register(pid, key)
    :ok
  end
  def talk(s) do 
    IO.puts("talk #{inspect [self(), s]}")
    send(key, [self(), s])
    receive do
      ["ok", x] -> x
    end    
  end
  def txs do talk(["txs"]) end
  def add_tx(tx) do 
    IO.puts("add tx")
    talk(["add_tx", tx]) 
  end
  def dump do send(key, ["dump"]) end
  def test do
    :ok=start
    IO.puts("0")
    {pub, priv}=Sign.new_key
    IO.puts("1")
    tx=["type": "spend"]
    tx=Sign.sign_tx(tx, pub, priv)
    IO.puts("2")
    add_tx(tx)
    IO.puts("3")
    IO.puts inspect txs
    dump
    IO.puts("4")
    IO.puts inspect txs		
  end
end
