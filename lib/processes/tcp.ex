defmodule Tcp do
	use Supervisor
	def atom_join(x, y) do
		a=to_string(x) <> "." <> to_string(y)
		String.to_atom(a)
	end
	def start_link(id, func1, func2) do
		import Supervisor.Spec, warn: false
		children = [
			worker(__MODULE__, [func1, id, "/:something", {0,0,0,0}], function: :run),
			worker(__MODULE__, [func2, :tcp_internal, "/priv/:something", {127,0,0,1}], function: :run, id: :tcp_internal), #cannot share a port!!
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
	def list2bin(x, out \\ "") do
		cond do
			x == [] -> out
			true -> list2bin(tl(x), out <> <<hd(x)>>)
		end
	end
	def de_list(x) do
		Enum.reduce(tl(x), hd(x), &(to_string(&2) <> "&" <> to_string(&1)))
	end
	def get_local(ip, port, x) do get(ip, port, x, '/priv/') end
	def get(ip, port, a, y \\ '/') do
		if is_list(a) do
			a = de_list(a)
		end
		false = is_tuple(a)
		url = 'http://' ++ to_char_list(ip) ++ ':' ++ to_char_list(to_string(port)) ++ y ++ to_char_list(Base.encode64(PackWrap.pack(a)))
		x = :httpc.request(url)
		case x do
			{:ok, z} ->  b = z |> elem(2) |> list2bin |> PackWrap.unpack
			{:error, :socket_closed_remotely} -> get(ip, port, a, y)
			_ -> {:error, :no_response}
		end
		#:jiffy.decode(x)
	end
end
defmodule Tcp.Handler do
	def init({:tcp, :http}, req, opts) do
		{:ok, req, opts}
	end
	def re_list(x, next \\ "", out \\ []) do
		cond do
			x == "" -> out ++ [next]
			true ->
				<< a::size(8), b::binary >> = x
				case <<a>> do
					"&" -> re_list(b, "", out ++ [next])
					y -> re_list(b, next <> y, out)
				end
		end
	end
	def handle(req, opts) do
		[func, ip] = opts
		f = fn(x) -> tl(x) end
		if ip == {127,0,0,1} do
			f = fn(x) -> tl(tl(tl(tl(tl(tl(x)))))) end
		end
		headers = [{"content-type", "text/plain"}]
		body = elem(req, 11) |> to_char_list |> f.() |> to_string |> Base.decode64!	|> PackWrap.unpack |> re_list |> func.() |> PackWrap.pack
		#body = :jiffy.encode(x)
		{:ok, resp} = :cowboy_req.reply(200, headers, body, req)
		{:ok, resp, func}
	end
	def terminate(_reason, _req, _state) do
		:ok
	end
end
