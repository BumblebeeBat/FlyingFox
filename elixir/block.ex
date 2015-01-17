defmodule Block do
  def load_block(h) do
    KV.get(to_string(h))
  end
  def start do
    KV.start
    Mempool.start
    genesis_block
    a=Accounts.empty
    bonds =                    100_000_000_000_000
    a = Dict.put(a, :amount, 2_000_000_000_000_000)
    a = Dict.put(a, :bond, bonds)     
    {creator_pub, creator_priv} = {"BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=", "pRxnT/38wyd6lSbwfCEVvchAL7m7AMYuZeQKrJW/RO0="}
    KV.put(creator_pub, a)
    KV.put("tot_bonds", bonds)
    create_sign
  end
  def genesis_block do
    new=[height: 0, txs: [], hash: ""]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def remove_block do
    h=KV.get("height")
    block=load_block(h)
    TxUpdate.txs_updates(block[:txs], -1)
    #give block creator his fee back.
    KV.put("height", h-1)
  end
  def smallest(l) do smallest(l, nil) end
  def smallest([], a) do a end
  def smallest(l, a) do
    smallest(tl(l), min(a, hd(l)))
  end
  def add_block(block) do
    true=Sign.verify_tx(block)
    {pub, sig, block}=block
    h=KV.get("height")
    h2=block[:height]
    ^h2=h+1
    IO.puts inspect block
    true=VerifyTx.check_txs(block[:txs])
    #run more checks
    #safety deposit per signer must be the same for each.
    #the poorest signer needs to be able to afford the safety deposit.
    #make sure block_hash matches previous block.
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    #make sure it has enough signers.
    signed_txs=(Enum.filter(block[:txs], fn(t) -> elem(t, 2)[:type]==:sign end))
    txs=Enum.map(signed_txs, fn(t) -> elem(t, 2) end)
    #IO.puts "txs #{inspect txs}"
    sent_funds=Enum.map(txs, fn(t) -> t[:amount] end)
    sent_funds = Enum.filter(block[:txs], fn(t) -> elem(t, 2)[:type]==:spend end)
    #IO.puts "sent funds #{inspect sent_funds}"
    sent_funds=Enum.reduce(sent_funds++[0], fn(a, b) -> a+b end)
    signers = Enum.map(signed_txs, fn(t) -> elem(t, 0) end)
    #IO.puts "signers:  #{inspect signers}"
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    #IO.puts "accs:  #{inspect accs}"
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
    num_signers= txs |> Enum.map(fn(t) -> length(t[:winners]) end) |> Enum.reduce(fn(a, b) -> a+b end)
    signer_bond=block[:bond_size]/num_signers
    #IO.puts "balances #{inspect balances}"
    sb=smallest(balances)
    #IO.puts "sb >= signer_bond = true, #{inspect sb} >= #{inspect signer_bond }"
    true = sb >= signer_bond
    KV.put(to_string(h2), block)
    txs = block[:txs] 
    #IO.puts inspect txs
    txs = Enum.filter(txs, fn(t) -> elem(t, 2)[:type]==:sign end)
    bs=block[:bond_size]
    #IO.puts "bs >= sent = true, #{inspect bs} >= #{inspect sent_funds }"
    true = bs>=sent_funds #maybe multiply sent_funds by 1.5?
    TxUpdate.txs_updates(block[:txs], 1, signer_bond)
    KV.put("height", h2)
  end
  def blockhash(height, txs) do
    prev_block=KV.get(height-1)
    DetHash.doit([prev_block[:hash], txs])
  end
  def buy_block do
    height=KV.get("height")
    {pub, priv}=Local.address
    txs=Mempool.txs
    bh=blockhash(height, txs)
    new=[height: height+1, txs: txs, hash: bh, bond_size: 100_000]
    new=Sign.sign_tx(new, pub, priv)
    add_block(new)
    Mempool.dump
  end
  def winners(balance, total, seed, pub) do
    Enum.filter(0..199, fn(x) -> VerifyTx.winner?(balance, total, seed, pub, x) end)
  end
  def create_sign do
    {pub, priv}=Local.address#Sign.new_key
    acc = KV.get(pub)
    prev = KV.get("height")
    tot_bonds = KV.get("tot_bonds")
    prev = prev-1
    prev = KV.get(prev)
    w=winners(acc[:bond], tot_bonds, VerifyTx.rng, pub)
    ran=:crypto.rand_bytes(10)
    secret=DetHash.doit(ran)
    tx=[type: :sign, prev_hash: prev[:hash], winners: w, secret_hash: secret]
    tx=Sign.sign_tx(tx, pub, priv)
    IO.puts "tx: #{inspect tx}"
    Mempool.add_tx(tx)
  end
  def test_sign do
    start
    IO.puts inspect Mempool.txs
    buy_block
  end
  def test_add_remove do
    start
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: :spend, amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    buy_block
    Mempool.add_tx(tx)
    buy_block
    IO.puts Accounts.balance(pub)
    0 |> load_block |> inspect |> IO.puts
    1 |> load_block |> inspect |> IO.puts
    2 |> load_block |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
    remove_block
    pub |> KV.get |> inspect |> IO.puts
  end
  def test_moneys do
    start
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: :spend2wait, amount: 300, fee: 100]
    #tx=[type: :spend, amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Enum.map(1..54, fn(x) -> buy_block end)
    acc=KV.get(pub)
    tx=[type: :wait2bond, wait_money: acc[:wait]]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    buy_block
    acc=KV.get(pub)
    0 |> load_block |> inspect |> IO.puts
    1 |> load_block |> inspect |> IO.puts
    2 |> load_block |> inspect |> IO.puts
    55 |> load_block |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
    acc=KV.get(pub)
    tx=[type: :bond2spend, amount: 179, fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    buy_block
    56 |> load_block |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
  end
end
