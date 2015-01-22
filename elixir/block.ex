defmodule Block do
  def load_block(h) do
    KV.get(to_string(h))[:block]
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
  end
  def genesis_block do
    new=[height: 0, txs: [], hash: ""]
    KV.put("height", 0)
    KV.put("0", new)
  end
  def txs_filter(txs, type) do 
    Enum.filter(txs, fn(t) -> elem(t, 2)[:type]==type end)
  end
  def num_signers(txs) do 
    txs_filter(txs, :sign)
    |> Enum.map(fn(t) -> length(elem(t, 2)[:winners]) end) 
    |> Enum.reduce(0, &(&1+&2))
  end
  def remove_block do
    h=KV.get("height")
    block=load_block(h)
    n=num_signers(txs_filter(block[:txs], :sign))
    TxUpdate.txs_updates(block[:txs], -1, div(block[:bond_size], n))
    #give block creator his fee back.
    KV.put("height", h-1)
  end
  def add_block(block) do
    true=Sign.verify_tx(block)
    {pub, sig, block}=block
    h=KV.get("height")
    h2=block[:height]
    ^h2=h+1
    true=VerifyTx.check_txs(block[:txs])
    #run more checks
    #safety deposit per signer must be the same for each.
    #the poorest signer needs to be able to afford the safety deposit.
    #make sure block_hash matches previous block.
    #block creator needs to pay a fee. he needs to have signed so we can take his fee.
    #make sure it has enough signers.
    sign_txs=txs_filter(block[:txs], :sign)
    send_txs = txs_filter(block[:txs], :spend)
    send_txs=Enum.map(send_txs, fn(t) -> elem(t, 2)[:amount] end)
    send_txs=Enum.reduce(send_txs, 0, &(&1+&2))
    signers = Enum.map(sign_txs, fn(t) -> elem(t, 0) end)
    accs = Enum.map(signers, fn(s) -> KV.get(s) end)
    balances = Enum.map(accs, fn(s) -> s[:bond] end)
    ns=num_signers(sign_txs)
    true=ns>43
    signer_bond=block[:bond_size]/ns
    sb = Enum.reduce(balances, nil, &(min(&1, &2)))
    true = sb >= signer_bond
    KV.put(to_string(h2), [block: block, meta: {}])
    txs = block[:txs] 
    txs = Enum.filter(txs, fn(t) -> elem(t, 2)[:type]==:sign end)
    bs=block[:bond_size]
    true = bs>=send_txs #maybe multiply send_txs by 1.5?
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
    Mempool.add_tx(tx)
    w
  end
  def test_reveal do
    w=create_sign
    buy_block
    h=KV.get("height")
    many buy_block
    old_block=KV.get(h)
    bond_size=old_block[:bond_size]
    tx=[signed_on: h, winners:  w, amount:length(w)*bond_size]
  end
  def test_rng do#need to test reveal first
    r=VerifyTx.rng()
    IO.puts inspect r
  end
  def test_sign do
    create_sign
    buy_block
    create_sign
    IO.puts "first block"
    buy_block
    IO.puts "second block"
  end
  def test_add_remove do
    create_sign
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: :spend, amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    buy_block
    IO.puts Accounts.balance(pub)
    0 |> load_block |> inspect |> IO.puts
    1 |> load_block |> inspect |> IO.puts
    2 |> load_block |> inspect |> IO.puts
    3 |> load_block |> inspect |> IO.puts
    remove_block
    pub |> KV.get |> inspect |> IO.puts
  end
  def test_moneys do
    {pub, priv}=Local.address#Sign.new_key
    tx=[type: :spend2wait, amount: 300, fee: 100]
    #tx=[type: :spend, amount: 55, to: "abcdefg", fee: 100]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    Enum.map(1..54, fn(x) -> create_sign
                             buy_block end)
    acc=KV.get(pub)
    tx=[type: :wait2bond, wait_money: acc[:wait]]
    tx=Sign.sign_tx(tx, pub, priv)
    Mempool.add_tx(tx)
    create_sign
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
    create_sign
    buy_block
    56 |> load_block |> inspect |> IO.puts
    pub |> KV.get |> inspect |> IO.puts
  end
end
