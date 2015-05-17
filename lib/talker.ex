defmodule Talker do
  #this module creates and maintains connections with multiple peers. It downloads blocks from them.
  #grabs the list of peers from peers thread.
  #has ability to black-list peers who are bad?
  #don't talke to same peers too frequently
  #don't ignore any peer much longer than the others.
  #download blocks and peers.
  use GenServer
  @name __MODULE__
  #@tcp_port Application.get_env :flying_fox, :tcp_port
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: @name])
  end
  def doit do
    GenServer.cast(@name, :doit)
  end
  def start do spawn_link(fn() -> timer end) end
  def still_on(blocks) do
    blocks == :ok or blocks == [] or (is_tuple(hd(blocks)) and :error in Dict.keys(blocks))
  end
  def add_peers(x) do
    Enum.map(x, fn(x) -> Peers.add_peer(x) end)
  end
  def download(n, i, p, out \\ []) do
    lo = length(out)
    cond do
      lo >= n -> out
      true ->
        out = out ++ Api.blocks(i+lo, i+n, p.port, p.ip)
        download(n - 1, i, p, out)
    end
  end
  def download_blocks(i, u, p) do
    blocks = download(min(50, u - i), i, p)
    my_block=Api.blocks(i, i)
    cond do
      my_block == [] ->
        BlockAbsorber.absorb(blocks)
        [status: :first_blocks]
      still_on(my_block) -> IO.puts("thread died")
      still_on(blocks) -> IO.puts("peer died 0")
      hd(my_block).data.hash == hd(blocks).data.hash ->
        BlockAbsorber.absorb(blocks)
        [status: :ahead]
      true ->
        blocks = download(50, max(0, i-40), p)
        BlockAbsorber.absorb(blocks)
        [status: :fork, height: u, peer: p]
    end
  end
  def trade_peers(p) do
    my_peers = Api.all_peers
    peers = Api.all_peers(p.port, p.ip)
    if my_peers == :ok or peers == :ok do
      IO.puts("peer died 1")
    else
      not_yours = Enum.filter(my_peers, &(not &1 in peers))
      not_mine = Enum.filter(peers, &(not &1 in my_peers))
      Enum.map(not_yours,&(Api.add_peer(elem(&1, 1),p.port,p.ip)))
      Enum.map(not_mine, &(Peers.add_peer(elem(&1, 1))))
    end
  end
  def check_peer(p) do #validating mode
    status = Api.status(p.port, p.ip)
		#IO.puts("status #{inspect status}")
    cond do
      #status == :ok or status == "ok"-> IO.puts("peer died 2 #{inspect p}")
			not is_list(status) -> IO.puts "status #{inspect status}"
      :error in Dict.keys(status) ->
        status[:error]
      true ->
        Peers.get(p)
        |> Map.put(:height, status[:height])
        |> Map.put(:hash, status[:hash])
        |> Peers.add_peer
        check_peer_2(p, status)
    end
  end
  def check_peer_2(p, status) do
    trade_peers(p)
    txs = Api.txs(p.port, p.ip)
		IO.puts("status: #{inspect status}")
    u = status[:height]
    i = KV.get("height")
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) ->
        IO.puts("tx error #{inspect txs}")
      u > i -> download_blocks(i, u, p)
      u == i -> Enum.map(txs, &(Mempool.add_tx(&1)))
      true ->
        IO.puts("im ahead")
        true
    end
  end
  def check_peers do
    Peers.get_all
    |> Enum.map(&(elem(&1,1)))
    |> Enum.map(&(spawn_link(fn -> check_peer(&1) end)))
   end
  def init(args) do
    start
    Enum.map(0..2, &(%Peer{ip: "localhost", port: Constants.tcp_port+&1})) 
    |> Enum.map(&(Peers.add_peer(&1)))
    {:ok, []}
  end
  def handle_cast(:doit, state) do
    check_peers
    {:noreply, state}
  end
  def timer do
    :timer.sleep(3000)#using a timer to stop crash on boot
    #IO.puts("talker")
    doit
    timer
  end
end
