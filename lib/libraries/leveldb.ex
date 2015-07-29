defmodule Leveldb do
	def db_lock(loc, f) do
		path = System.cwd <> "/dbs" #different in windows?
		File.mkdir(path)
		x = Exleveldb.open(path <> loc)
		IO.puts("leveldb #{inspect x}")
		{status, db} = x
		case status do
			:ok ->
				#we have a lock on the db, so no one else can use it till we are done.
				out = f.(db)
				Exleveldb.close(db)#unlock
				out
			:error ->#someone else has a lock on the db.
				:timer.sleep(5)
				db_lock(loc, f)
			true -> IO.puts("db broke")
		end
	end
	def put(key, val, loc) do Task.start_link(fn() -> db_lock(loc, fn(db) -> Exleveldb.put(db, key, PackWrap.pack(val), []) end) end) end
	def get(key, loc) do db_lock(loc, fn(db) -> Exleveldb.get(db, key, []) end) |> elem(1) |> PackWrap.unpack end
end
