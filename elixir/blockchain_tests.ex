defmodule BlockchainTests do
  def test(n \\ 10) do
    cond do
      n<0 -> nil
      true ->
        Blockchain.create_sign
        Blockchain.create_reveal
        Blockchain.add_block(BlockchainPure.buy_block)
        test(n-1)
    end
  end
  def test_reveal do
    {pub, priv}=Local.address
    w=Blockchain.create_sign
    Blockchain.add_block(BlockchainPure.buy_block)
    Enum.map(1..5, fn(x) -> Blockchain.create_sign
                            Blockchain.add_block(BlockchainPure.buy_block) end)
    Blockchain.create_reveal
    Blockchain.create_sign
    Blockchain.add_block(BlockchainPure.buy_block)
    h=KV.get("height")
    IO.puts inspect h
  end
  def test_rng do#need to test reveal first
    r=VerifyTx.rng()
    IO.puts inspect r
  end
  def test_sign do
    Blockchain.create_sign
    Blockchain.add_block(BlockchainPure.buy_block)
    Blockchain.create_sign
    IO.puts "first block"
    Blockchain.add_block(BlockchainPure.buy_block)
    IO.puts "second block"
  end
  def test_add_remove do
    Blockchain.create_sign
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: "spend", amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Blockchain.add_block(BlockchainPure.buy_block)
    acc=KV.get(pub)
    IO.puts Dict.get(acc, :amount)
    f=&(BlockchainPure.get_block(&1))
    f.(0) |> inspect |> IO.puts
    f.(1) |> inspect |> IO.puts
    f.(2) |> inspect |> IO.puts
    f.(3) |> inspect |> IO.puts
    Blockchain.remove_block
    pub |> KV.get |> inspect |> IO.puts
  end
  def test_moneys do
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: "spend2wait", amount: 300, fee: 100]
    #tx=[type: :spend, amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Enum.map(1..VerifyTx.epoch, fn(x) -> Blockchain.create_sign
                             Blockchain.add_block(BlockchainPure.buy_block) end)
    acc=KV.get(pub)
    tx=[type: "wait2bond", wait_money: acc[:wait]]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Blockchain.create_sign
    Blockchain.add_block(BlockchainPure.buy_block)
    acc=KV.get(pub)
    f=&(Blockchain.get_block(&1))
    f.(0) |> inspect |> IO.puts
    f.(1) |> inspect |> IO.puts
    f.(3) |> inspect |> IO.puts
    f.(55) |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
    acc=KV.get(pub)
    tx=[type: "bond2spend", amount: 179, fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Blockchain.create_sign
    Blockchain.add_block(BlockchainPure.buy_block)
    56 |> Blockchain.get_block |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
  end
end
