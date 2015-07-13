defmodule Talker do
  #this module creates and maintains connections with multiple peers. It downloads blocks from them.
  #grabs the list of peers from peers thread.
  #has ability to black-list peers who are bad?
  #don't talke to same peers too frequently
  #don't ignore any peer much longer than the others.
  #download blocks and peers.
  use GenServer
  @name __MODULE__
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: @name])
  end
  def start do spawn_link(fn() -> timer end) end
  def still_on(blocks) do blocks == :ok or blocks == [] or (is_tuple(hd(blocks)) and :error in Dict.keys(blocks)) end
  def add_peers(x) do Enum.map(x, fn(x) -> Peers.add_peer(x) end) end
	def flip(x) do flip(x, []) end
	def flip([], x) do x end
	def flip([head|tail], out \\ []) do flip(tail, [head|out]) end
  def download_blocks(i, u, p) do
    blocks = Cli.blocks(min(50, u - i), i, p) |> flip
		if blocks != [] do
			parent = hd(blocks).data.hash |> KV.get
			#1 block at a time is pretty conservative, we could probably do this faster.
			if parent == nil do download_blocks(i-5, i, p) end 
			BlockAbsorber.absorb(blocks)
		end
  end
  def trade_peers(p) do
		keys = fn(z) -> Enum.map(z, fn(x) -> Peers.peer_key(x) end) end
		my_peers = Cli.all_peers
    peers = Cli.all_peers(p)
		my_keys = keys.(my_peers)
		peers_keys = keys.(peers)
    if my_peers == :ok or peers == :ok do
      IO.puts("peer died 1")
    else
      not_yours = Enum.filter(my_peers, &(not Peers.peer_key(&1) in peers_keys))
      not_mine = Enum.filter(peers, &(not Peers.peer_key(&1) in my_keys))
      Enum.map(not_yours,&(Cli.add_peer(&1,p)))
      Enum.map(not_mine, &(Peers.add_peer(&1)))
    end
  end
  def check_peer(p) do
		#IO.puts("check peer #{inspect p}")
    status = Cli.status(p)
    cond do
			not is_map(status) -> status
      status.height > 0 and is_number(status.height) ->
        x = Peers.get(p)
        |> Map.put(:height, status.height)
        |> Map.put(:hash, status.hash)
				x |> Peers.add_peer
        check_peer_2(x, status)
			true -> IO.puts("nothing to do")
    end
  end
  def check_peer_2(p, status) do
		#IO.puts("check peer 2 #{inspect p}")
    trade_peers(p)
    txs = Cli.txs(p)
    u = status.height
    i = KV.get("height")
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) ->
        IO.puts("tx error #{inspect txs}")
      u > i -> download_blocks(i, u, p)
      u == i ->
				Enum.map(txs, &(Mempool.add_tx(&1)))
				Cli.txs |> Enum.filter(&(not &1 in txs)) |> Enum.map(&(Cli.pushtx(&1, p)))
      true ->
				#should push blocks!
				Enum.map((u+1)..min((u+4), i), &(Blockchain.get_block(&1)))
				|> Cli.add_blocks(p)
        IO.puts("im ahead")
        true
    end
  end
  def check_peers do
    Cli.all_peers
    |> Enum.map(&(spawn_link(fn -> check_peer(&1) end)))
   end
  def init(_) do
    start
    Enum.map(0..Constants.max_nodes, &(%Peer{ip: "localhost", port: Constants.tcp_port+&1})) 
    |> Enum.map(&(Peers.add_peer(&1)))
    Enum.map(0..Constants.max_nodes, &(%Peer{ip: "192.241.212.114", port: Constants.tcp_port+&1})) 
    |> Enum.map(&(Peers.add_peer(&1)))
		#Peers.add_peer(%Peer{ip: "192.241.212.114", port: 6669})
    {:ok, []}
  end
  def doit do GenServer.cast(@name, :doit) end
  def handle_cast(:doit, state) do
    check_peers
    {:noreply, state}
  end
  def timer do
    :timer.sleep(3000)
    doit
    timer
  end
end
