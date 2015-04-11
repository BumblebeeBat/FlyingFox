defmodule Talker do
  #this module creates and maintains connections with multiple peers. It downloads blocks from them.
  #grabs the list of peers from peers thread.
  #has ability to black-list peers who are bad?
  #never talks to peers more often than a minimum.
  #don't ignore any peer much longer than the others.
  #download blocks and peers.
  def peers do 
  [[ip: "localhost", port: 6664],
   [ip: "localhost", port: 6665],
   [ip: "localhost", port: 6666]]
  end
  def add_peers(x) do Enum.map(x, fn(x) -> Peers.add_peer(x) end) end
  def download_blocks(i, u, p) do
    cond do
      u>i and same_branch -> #only download in the direction we are currently searching. Depth first search.
        IO.puts("im behind")
        blocks=Api.blocks(i+1, i+100, p[:port], p[:ip])
        IO.puts("blocks #{inspect blocks}")
        BlockAbsorber.poly_absorb(blocks)
      u>i and off_branch ->
    end
  end
  def check_peer(p) do #validating mode
    status = Api.status(p[:port], p[:ip])
    cond do
      status == :ok -> ":ok error"
      :error in Dict.keys(status) -> status[:error]
      true -> check_peer_2(p, status)
    end
  end
  def 
  def check_peer_2(p, status) do
    my_peers = Api.all_peers
    peers = Api.all_peers(p[:port], p[:ip])
    not_yours = Enum.filter(my_peers, &(not &1 in peers))
    not_mine = Enum.filter(peers, &(not &1 in my_peers))
    Enum.map(not_yours, &(Api.add_peer(elem(&1, 1),p[:port],p[:ip])))
    Enum.map(not_mine, &(Peers.add_peer(elem(&1, 1))))
    txs=Api.txs(p[:port], p[:ip])
    peer=Peers.get(p)
    peer = Dict.put(peer, :height, status[:height])
    peer = Dict.put(peer, :hash, status[:hash])
    Peers.add_peer(peer)
    u=status[:height]
    i=KV.get("height")
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) -> 
        IO.puts('tx error')
        txs[:error]
      u>i -> download_blocks(i, u, p)
      u==i -> Enum.map(txs, &(Mempool.add_tx(&1)))
      true -> true
    end
  end
  def check_peers do
    #my_status = Api.status(KV.get("port"), "localhost")
    prs = Enum.map(Peers.get_all, &(elem(&1,1)))
    Enum.map(prs, &(Task.start_link(fn -> check_peer(&1) end)))
  end
  def looper do#change this so that the program has enough memory to search the tree of block-chains without retracing its steps too much.
    #if below the weak subjectivity hash, then head towards it.
    #otherwise find the highest valid node without leaving the weakly subjective part of the tree.
    check_peers
    :timer.sleep(5000)
    looper
  end
  def start do
    add_peers(peers)
    Task.start_link(fn() -> looper end)
  end
end
