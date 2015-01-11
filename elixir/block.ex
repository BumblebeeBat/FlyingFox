defmodule Block do
  def load_block(h) do
    KV.get(to_string(h))
  end
  def start do
    KV.start
    Mempool.start
    genesis_block
  end
  def genesis_block do
    new=[height: 0, txs: []]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def add_block(block) do
    h=KV.get("height")
    h2=block[:height]
    ^h2=h+1
    true=VerifyTx.check_txs(block[:txs])
    KV.put(to_string(h2), block)
    TxUpdate.txs_updates(block[:txs])
  end
  def buy_block do
    height=KV.get("height")
    new=[height: height+1, txs: Mempool.txs]
    add_block(new)
    Mempool.dump
  end
  def test do
    start
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: :spend, amount: 55, to: "abcdefg"]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    buy_block
    IO.puts Accounts.balance(pub)
    0 |> load_block |> inspect |> IO.puts
    1 |> load_block |> inspect |> IO.puts
  end
end
