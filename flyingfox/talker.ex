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
  def check_peer(p, my_status) do #validating mode
    status = Api.status(p[:port], p[:ip])
    IO.puts("status #{inspect status}")
    cond do
      status == :ok -> ":ok error"
      :error in Dict.keys(status) -> status[:error]
      true -> check_peer_2(p, status, my_status)
    end
  end
  def check_peer_2(p, status, my_status) do
    txs=Api.txs
    peer=Peers.get(p)
    peer = Dict.put(peer, :height, status[:height])
    peer = Dict.put(peer, :hash, status[:hash])
    Peers.update(peer)
    u=status[:height]
    i=KV.get("height")
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) -> 
        IO.puts('tx error')
        txs[:error]
      u>i -> 
        IO.puts("im behind")
        blocks=Api.blocks(i+1, i+100, p[:port], p[:ip])
        IO.puts("blocks #{inspect blocks}")
        BlockAbsorber.poly_absorb(blocks)
      u==i -> Enum.map(txs, &(Mempool.add_tx(&1)))
      true -> true
    end
  end
  def check_peers do
    my_status = Api.status(KV.get("port"), "localhost")
    prs = Peers.get_all
    #IO.puts("prs: #{inspect prs}")
    Enum.map(prs, fn(p) -> Task.start_link(fn -> 
      {a, b} = p
      check_peer(b, my_status) end) 
    end)
  end
  def looper do
    check_peers
    :timer.sleep(3000)
    looper
  end
  def start do
    add_peers(peers)
    Task.start_link(fn() -> looper end)
  end
end
