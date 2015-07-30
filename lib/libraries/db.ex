#channel manager needs the Exleveldb removed too.
defmodule DB do
	def put_lock(loc, g) do
		path = System.cwd <> "/dbs" #different in windows?
		File.mkdir(path)
		{status, pid} = File.open(path <> loc, [:write])
		case status do
			:ok ->
				#we have a lock on the db, so no one else can use it till we are done.
				g.(pid)
				File.close(pid)#unlock
			:error ->#someone else has a lock on the db.
				:timer.sleep(5)
				put_lock(loc, g)
			_ -> IO.puts("db broke")
		end
	end
	def put_function(loc, f) do Task.start_link(fn() -> put_lock(loc, fn(pid) -> IO.binwrite(pid, PackWrap.pack(f.()))	end) end)	end
	def get_raw(loc) do
		path = System.cwd <> "/dbs" #different in windows?
		File.mkdir(path)
		bin = File.read(path <> loc)
		cond do
			bin == {:error, :enoent} ->
				File.touch(path <> loc)
				get_raw(loc)
			bin == {:ok, ""} -> ""
			true ->
				bin |> elem(1) |> PackWrap.unpack
		end
	end
	def test do
		loc = "/test"
		put_function(loc, fn() -> "test23434" end)
		:timer.sleep(100)
		IO.puts("test #{inspect get_raw(loc)}")
	end
#^^^^^ only using above this line for now.
	def put_get(loc) do
		current = get_raw(loc)
		if current == "" do current = %{} end
		current
	end
	def put_dict(loc, key, value) do
		put_function(loc, fn(db) ->
			current = put_get(loc)
			Dict.put(current, key, value)
		end)
	end
	def get_dict(loc, key) do
		current = put_get(loc)
		Dict.get(current, key)
	end
	def test_not_yet do
		loc = "/test"
		put_dict(loc, "a", "abc")
		a = get_dict(loc, "a")
		IO.puts("test #{inspect a}")
	end
end
