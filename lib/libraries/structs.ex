defmodule DeleteAccount do
	defstruct pub: ""
end
defmodule SendMessage do
	defstruct pub: "", to: "", msg: "", payment: %ChannelBlock{}
end
defmodule MsgPop do
	defstruct msg: "", payment: %ChannelBlock{}
end
defmodule PopMessage do
	defstruct pub: ""
end
defmodule InboxSize do
	defstruct pub: ""
end


defmodule Status do
  defstruct height: 0, hash: "", pubkey: ""
end
defmodule Account do
  #wait={amount, height}
  #defstruct amount: 0, bond: 0, wait: {0, 0}, nonce: 0
  defstruct amount: 0, bond: 0, nonce: 0, wait_amount: 0, wait_height: 0
end
defmodule Block do
  defstruct height: 0, txs: [], hash: "", bond_size: 1.0e10, pub: ""
end
defmodule Channel do
  defstruct pub: nil, pub2: nil, amount: 0, amount2: 0, time: 0, nonce: 0, delay: 10, bets: []
end
defmodule Peer do
  defstruct time: 0, height: 0, hash: nil, ip: nil, port: 0
end
