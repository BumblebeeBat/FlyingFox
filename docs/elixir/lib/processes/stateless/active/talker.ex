#maybe this should be split into more smaller threads. One can focus on finding out peer's heights, one can download blocks, etc.

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
  def still_on(blocks) do blocks == :ok or blocks == [] or (is_tuple(hd(blocks)) and :error in Dict.keys(blocks)) end
  def add_peers(x) do Enum.map(x, fn(x) -> Peers.add_peer(x) end) end
	def flip(x) do flip(x, []) end
	def flip([], x) do x end
	def flip([head|tail], out \\ []) do flip(tail, [head|out]) end
  def download_blocks(i, u, p) do
		blocks = Cli.blocks(i, min(i+50, u), p) |> flip
		if blocks != [] do
			parent = hd(blocks).data.hash |> KV.get
			#Task.start_link(fn() -> BlockAbsorber.absorb(blocks) end)
			BlockAbsorber.absorb(blocks)
			if parent == nil do download_blocks(i-5, i, p) end 
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
  def check_peer(p, n) do
		#first we should check if the peer has been connected to before. If we never connected before, then only connect to them with probability 10%.
		#IO.puts("check peer #{inspect p}")
		:random.seed(:erlang.now)
		r = trunc(10*:random.uniform)
		cond do
			p.height == 0 and r > 0 -> nil
			true -> check_peer_1(p, n)
		end
	end
	def check_peer_1(p, n) do
    status = Cli.status(p)
    cond do
			not is_map(status) -> status
      status.height > 0 and is_number(status.height) ->
        x = Peers.get(p)
        |> Map.put(:height, status.height)
        |> Map.put(:hash, status.hash)
				x |> Peers.add_peer
        check_peer_2(x, status, n)
			true -> IO.puts("nothing to do")
    end
  end
	def push_till(u, i, p) do
		Enum.map((u+1)..min((u+5), i), &(Blockchain.get_block(&1)))
		|> Cli.add_blocks(p)
		IO.puts("push blocks #{inspect u} #{inspect i}")
		if u+5 < i do push_till(u+5, i, p) end
	end
  def check_peer_2(p, status, n) do
		#IO.puts("check peer 2 #{inspect p}")
    trade_peers(p)
    txs = Cli.txs(p)
    u = status.height
    i = KV.get("height")
		hash = Blockchain.blockhash(Blockchain.get_block(u))
		often = rem(n, 6) == 0
    cond do
      txs == :ok -> IO.puts("txs shouldn't be :ok")
      (txs != []) and length(txs)>0 and is_tuple(hd(txs)) ->
        IO.puts("tx error #{inspect txs}")
      u > i ->
				IO.puts("download blocks")
				download_blocks(i, u, p)
      u == i ->
				Enum.map(txs, &(Mempool.add_tx(&1)))
				Cli.txs |> Enum.filter(&(not &1 in txs)) |> Enum.map(&(Cli.pushtx(&1, p)))
			status.hash == hash and often ->
				Enum.map((u+1)..min((u+50), i), &(Blockchain.get_block(&1)))
				|> Cli.add_blocks(p)
	    often ->
				Enum.map((u-20)..min(u, i), &(Blockchain.get_block(&1)))
				|> Cli.add_blocks(p)
			true -> true
    end
  end
  def check_peers(n) do
    Cli.all_peers
    |> Enum.map(&(Task.start_link(fn -> check_peer(&1, n) end)))
   end
  def init(_) do
		Task.start_link(fn() -> timer end)
		Constants.server |> Peers.add_peer
    {:ok, 0}
  end
  def doit do GenServer.cast(@name, :doit) end
  def handle_cast(:doit, n) do
    check_peers(n)
    {:noreply, n+1}
  end
  def timer do
		before = KV.get("height")
    :timer.sleep(1000)
		a = KV.get("height")
		if a == before do doit end
    timer
  end
end
