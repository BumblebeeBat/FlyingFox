defmodule Block do
	def load_block(h) do
	  KV.get(to_string(h))
	end
	def start do
		KV.start
	end
	def genesis_block do
		new=[height: 0, txs: []]
		KV.put("height", 0)
		KV.put("0", new)
	end
	def add_block(block) do
		h=KV.get("height")
		h2=block[:height]
		^h2=h+1
		true=VerifyTx.check_txs(block[:txs])
		#are the txs all valid?
		KV.put(to_string(h2), block)
		#move money around for each tx
	end
	def grow_chain(height) do
		prev=load_block(height)
		new=[height: hd(prev)+1, txs: []]
		KV.put(to_string(height+1), new)
	end
	def height(block) do
		hd(block)
	end
end
