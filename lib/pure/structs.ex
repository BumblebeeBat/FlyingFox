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
defmodule Peer do
  defstruct time: 0, height: 0, hash: nil, ip: nil, port: 0
end
