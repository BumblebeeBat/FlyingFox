defmodule CheckLogic do
	def main(tx, txs, prevhash) do
		k = tx.data.__struct__
		f = case k do
					:Elixir.Sign -> &(Sign.check(&1, &2, prevhash))
					:Elixir.Spend -> &(Spend.check(&1, &2))
					:Elixir.Spend2Wait -> &(Spend2Wait.check(&1, &2))
					:Elixir.Wait2Bond -> &(Wait2Bond.check(&1, &2))
					:Elixir.Bond2Spend -> &(Bond2Spend.check(&1, &2))
					:Elixir.Slasher -> &(Spend.check(&1, &2))
					:Elixir.Reveal -> &(Reveal.check(&1, &2))
					:Elixir.ToChannel -> &(ToChannel.check(&1, &2))
					:Elixir.ChannelBlock -> &(ChannelBlock.check(&1, &2))
					:Elixir.CloseChannel -> &(CloseChannel.check(&1, &2))
					:Elixir.Oracle -> &(Oracle.check(&1, &2))
					:Elixir.Judgement -> &(Judgement.check(&1, &2))
					:Elixir.Win -> &(Win.check(&1, &2))
					_ ->
						IO.puts("invalid tx type: #{inspect k}")
					  fn(_, _) -> false end
			end
		cond do
			not f.(tx, txs) ->
				#IO.puts("bad tx")
				false
			not CryptoSign.verify_tx(tx) ->
				IO.puts("bad signature")
				false
			true -> true
		end
	end
end
