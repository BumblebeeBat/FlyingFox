defmodule TxCreator do
  def nonce(pub) do 
    a=Mempool.txs |> Enum.filter(fn(tx) -> tx[:pub]==pub end) |> length
    a+KV.get(pub)[:nonce] 
  end
  def spend(amount, to) do
    {pub, priv}=Keys.address
    balance = KV.get(pub)[:amount]
    if balance<amount do IO.puts("warning, you cannot afford to spend this tx, so it wont be valid") end
    tx=[type: "spend", to: to, amount: amount, nonce: nonce(pub), fee: 10000]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)    
  end
  def sign do
    {pub, priv}=Keys.address
    acc = KV.get(pub)
    prev = KV.get("height")
    tot_bonds = KV.get("tot_bonds")
    prev = KV.get(to_string(prev))
    w=Enum.filter(0..Constants.chances_per_address, fn(x) -> VerifyTx.winner?(acc[:bond], tot_bonds, VerifyTx.rng, pub, x) end) 
    ran=:crypto.rand_bytes(10)
    h=KV.get("height")+1
    KV.put("secret #{inspect h}", ran)
    secret=DetHash.doit(ran)
    tx=[type: "sign", prev_hash: prev[:data][:hash], winners: w, secret_hash: secret, nonce: nonce(pub)]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
  end
  def reveal do
    {pub, priv}=Keys.address
    h=KV.get("height")-Constants.epoch
    cond do
      h<1 -> nil
      true ->
        old_block=BlockchainPure.get_block(h)
        old_tx = old_block[:txs] |> Enum.filter(&(&1[:data][:type]=="sign")) |> Enum.filter(&(&1[:pub]==pub)) |> hd
        w=old_tx[:data][:winners]
        bond_size=old_block[:bond_size]
        tx=[type: "reveal", signed_on: h, winners: w, amount: length(w)*bond_size, secret: KV.get("secret #{inspect h}"), nonce: nonce(pub)]
        tx=Sign.sign_tx(tx, pub, priv)
        Mempool.add_tx(tx)
    end
  end  
end
