defmodule Tcp do
	use Supervisor
	def atom_join(x, y) do
		a=to_string(x) <> "." <> to_string(y)
		String.to_atom(a)
	end
	def start_link(id, func1, func2) do
		import Supervisor.Spec, warn: false
		children = [
			worker(__MODULE__, [func1, id, "/", {0,0,0,0}], function: :run),
			worker(__MODULE__, [func2, :tcp_internal, "/priv/", {127,0,0,1}], function: :run, id: :tcp_internal), 
		]
		opts = [strategy: :one_for_one, name: Tcp.Supervisor]
		Supervisor.start_link(children, opts)
	end
	def run(func, id, r, ip) do
		routes = [ {r, Tcp.Handler, [func, ip]}	]
		dispatch = :cowboy_router.compile([{:_, routes}])
		p = Port.port
		if id == :tcp_internal do p = p+1000 end
		opts = [port: p, ip: ip]
		env = [env: [dispatch: dispatch]]
		{:ok, _pid} = :cowboy.start_http(atom_join(:http, id), 100, opts, env)
	end
	def get_local(ip, port, x) do get(ip, port, x, '/priv') end
	def get(ip, port, a, y \\ '') do
		url = 'http://' ++ to_char_list(ip) ++ ':' ++ to_char_list(to_string(port)) ++ y
		content_type = 'application/octet-stream'
		request = {url, [], content_type, PackWrap.pack(a)}
		#:httpc.request(:post, request, [{:timeout, 2000}], [])#hopefully 2 seconds?
		x = :httpc.request(:post, request, [], [])
		case x do
			{:ok, z} ->
				b = z |> elem(2)
				if b == [] do [] else PackWrap.unpack(b) end
			{:error, :socket_closed_remotely} -> get(ip, port, a, y)
			{:error, {:failed_connect, _}} -> nil
			x ->
				IO.puts("TCP WEIRD #{inspect x}")
				x
		end
	end
end
defmodule Tcp.Handler do
	def init({:tcp, :http}, req, opts) do
		{:ok, req, opts}
	end
	def body(req, opts) do
		[func, ip] = opts
		{status, data, req2} = :cowboy_req.body(req, opts)
		cond do
			status == :ok -> data
			true ->
				:timer.sleep(50)
				body(req, opts)
		end
	end
	def handle(req, opts) do
		[func, ip] = opts
		f = fn(x) -> tl(x) end
		if ip == {127,0,0,1} do
			f = fn(x) -> tl(tl(tl(tl(tl(tl(x)))))) end
		end
		headers = [{"content-type", "application/octet-stream"},
							 {"Access-Control-Allow-Origin", "*"}]
		#{"Access-Control-Allow-Methods", "GET, POST"},
		#{"Access-Control-Allow-Credentials", "true"}]

		b = body(req, opts) |> PackWrap.unpack |> func.() |> PackWrap.pack
		{:ok, resp} = :cowboy_req.reply(200, headers, b, req)
		{:ok, resp, opts}
	end
	def terminate(_reason, _req, _state) do
		:ok
	end
end
