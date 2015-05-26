defmodule Account do
  #wait={amount, height}
  defstruct amount: 0, bond: 0, wait: {0, 0}, nonce: 0
end

defmodule Signed do
  defstruct sig: nil, pub: nil, data: nil, meta: nil
end

defmodule Block do
  defstruct height: 0, txs: [], hash: "", bond_size: 1.0e10
end

defmodule Channel do
  @epoch Application.get_env :flying_fox, :epoch  
  defstruct pub: nil, pub2: nil, amount: 0, amount2: 0, time: 0, nonce: 0, delay: @epoch
end

defmodule RevealTx do
  defstruct nonce: 0, signed_on: 0, winners: [], amount: 0, secret: nil, bond_size: 0
end

defmodule SpendTx do
  defstruct nonce: 0, to: "", amount: 0, fee: 10000
end

defmodule SignTx do
  defstruct nonce: 0, height: 0, secret_hash: nil, winners: [], prev_hash: nil
end

defmodule Spend2WaitTx do
  defstruct nonce: 0, amount: 0
end

defmodule Wait2BondTx do
  defstruct nonce: 0, wait_money: 0
end

defmodule Bond2SpendTx do
  defstruct nonce: 0
end

defmodule SlasherTx do
  defstruct nonce: 0, tx1: nil, tx2: nil, signed_on: 0
end

defmodule ToChannelTx do
  defstruct nonce: 0, to: :pub, channel: nil
end

defmodule ChannelBlockTx do
  defstruct nonce: 0, channel: nil, amount: 0, amount2: 0, pub: nil, pub2: nil, secret_hash: nil
end

defmodule CloseChannelTx do
  defstruct nonce: 0, channel: nil, channel_block: nil
end

defmodule OracleTx do
	defstruct ins: [], sigs: [], oracle_id: ""
end

defmodule JudgementTx do
	defstruct bet: "", result: "", sigs: []
end

defmodule Peer do
  defstruct time: 0, height: 0, hash: nil, ip: nil, port: 6666
end
