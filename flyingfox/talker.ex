defmodule Talker do
  #this module creates and maintains connections with multiple peers. It downloads blocks from them.
  #grabs the list of peers from peers thread.
  #has ability to black-list peers who are bad?
  #never talks to peers more often than a minimum.
  #don't ignore any peer much longer than the others.
  #download blocks and peers.
  def add_peers(x) do Enum.map(x, fn(x) -> Peers.add_peer(x) end) end
  def absorb(p, i) do
    blocks=Api.blocks(i+1, i+100, p[:port], p[:ip])
    BlockAbsorber.poly_absorb(blocks)
  end
  def still_on(blocks) do blocks == :ok or (is_tuple(hd(blocks)) and :error in Dict.keys(blocks)) end
  def download_blocks(i, u, p) do
    blocks=Api.blocks(i, i+100, p[:port], p[:ip])
    my_block=Api.blocks(i, i)
    cond do
      my_block == [] ->
        absorb(p, i)
        [status: :first_blocks]
      still_on(my_block) -> IO.puts("thread died")
      still_on(blocks) -> IO.puts("peer died")
      hd(my_block)[:data][:hash] == hd(blocks)[:data][:hash] ->
        absorb(p, i)
        [status: :ahead]
      true -> 
        IO.puts("off branch")
        [status: :fork, height: u, peer: p]
    end
  end
  def trade_peers(p) do
    my_peers = Api.all_peers
    peers = Api.all_peers(p[:port], p[:ip])
    if my_peers == :ok or peers == :ok do
      IO.puts("peer died")
    else
      not_yours = Enum.filter(my_peers, &(not &1 in peers))
      not_mine = Enum.filter(peers, &(not &1 in my_peers))
      Enum.map(not_yours, &(Api.add_peer(elem(&1, 1),p[:port],p[:ip])))
      Enum.map(not_mine, &(Peers.add_peer(elem(&1, 1))))
    end
  end
  def check_peer(p) do #validating mode
    status = Api.status(p[:port], p[:ip])
    cond do
      status == :ok -> IO.puts("peer died")
      :error in Dict.keys(status) ->
        #IO.puts("check peer error #{inspect status}")
        status[:error]
      true -> 
        Peers.get(p) |> Dict.put(:height, status[:height]) |> Dict.put(:hash, status[:hash]) |> Peers.add_peer
        check_peer_2(p, status)
    end
  end
  def check_peer_2(p, status) do
    trade_peers(p)
    txs=Api.txs(p[:port], p[:ip])
    u=status[:height]
    i=KV.get("height")
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) -> 
        IO.puts("tx error #{inspect txs}")
      u>i -> download_blocks(i, u, p)
      u==i -> Enum.map(txs, &(Mempool.add_tx(&1)))
      true ->
        IO.puts("im ahead")
        true
    end
  end
  def check_peers do
    #my_status = Api.status(KV.get("port"), "localhost")
    Peers.get_all 
    |> Enum.map(&(elem(&1,1))) 
    |> Enum.map(&(spawn_link(fn -> check_peer(&1) end)))
   end
  def looper do#change this so that the program has enough memory to search the tree of block-chains without retracing its steps too much.
    #if below the weak subjectivity hash, then head towards it.
    #otherwise find the highest valid node without leaving the weakly subjective part of the tree.
    x = round(:random.uniform()*10+10)
    Enum.map(1..x, fn(_) ->
      check_peers
      :timer.sleep(3000)
      #spawn_link(fn() -> Blockchain.sign_reveal end)
    end)
    looper
  end
  def start do
    peers = Enum.map(0..2, &([ip: "localhost", port: 6664+&1]))
    add_peers(peers)
    pid = spawn_link(fn() -> looper end)
    pid
  end
end
