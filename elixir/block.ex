defmodule Block do
	def load_block(h) do
		deserialize(KV.get(to_string(h)))
	end
	def start do
		KV.start
	end
	def serialize(block) do
		{:ok, out}=MessagePack.pack(block)
		out
	end
	def deserialize(block) do
		{:ok, out}=MessagePack.unpack(block)
		out
	end
	def genesis_block do
		new=[0]
		KV.put("0", serialize(new))
	end
	def new_block(height) do
		prev=load_block(height)
		new=[hd(prev)+1]
		KV.put(to_string(height+1), serialize(new))
	end
	def height(block) do
		hd(block)
	end
end
