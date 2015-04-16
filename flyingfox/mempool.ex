defmodule Mempool do
  def looper(mem) do
    receive do
      ["dump"] -> mem=[]
      ["add_tx", tx, s] ->
        cond do
          VerifyTx.check_tx(tx, mem) -> 
            send(s, ["ok", "ok"])
            mem=[tx|mem]
          true -> 
            send(s, ["ok", "bad tx"])
        end
      ["txs", s] ->
        send(s, ["ok", mem])
      x -> IO.puts("looper weird #{inspect x}")
    end
    looper(mem)
  end
  def key do :txs end
  def start do
    pid = spawn_link(fn -> looper([]) end)
    Process.register(pid, key)
    pid
  end
  def talk(s) do 
    send(key, s)
    receive do
      ["ok", x] -> x
    end    
  end
  def txs do talk(["txs", self()]) end
  def add_tx(tx) do talk(["add_tx", tx, self()]) end
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
