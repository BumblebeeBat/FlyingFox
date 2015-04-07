Flying Fox~

This is a proof of stake blockchain in elixir, it is not yet complete.

These files have state:
block_absorber.ex
mempool.ex
peers.ex
kv.ex

Everything that looks like Elixir.*.beam is a program compiled to bytecode for the erlang virtual machine.

example:

$ iex
iex(1)>Main.start
{:ok, #PID<0.63.0>}
iex(2)> BlockAbsorber.buy_block
:ok
iex(3)> KV.get("1")
[meta: [revealed: []],
 pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=",
 sig: "MEYCIQDy04k/dLzgGZxozZ6NSv3cJVSnj4iApxW/1yXOBznmkAIhALEFlZmoCXBSNPKL447Q7Y0idLfv9hag11KvsJ22fm6+",
 data: [height: 1,
  txs: [[pub: "BCmhaRq42NNQe6ZpRHIvDxHBThEE3LDBN68KUWXmCTKUZvMI8Ol1g9yvDVTvMsZbqHQZ5j8E7sKVCgZMJR7lQWc=",
    sig: "MEUCIQCgS7ph9JC4slzP/lVsaEzJliSj5V/qEUkcTlbuujNmpgIgUS9FnVQL1qhvW756WXrth4i2IbTZ6KsIRoCGgFLiQFs=",
    data: [type: "sign", prev_hash: nil,
     winners: [0, 1, 2, 4, 5, 6, 8, 10, 12, 14, 16, 17, 20, 24, 26, 27, 30, 31,
      32, 33, 36, 37, 38, 39, 43, 44, 46, 47, 48, 50, 52, 53, 55, 56, 58, 61,
      62, 63, 67, ...],
     secret_hash: "3p31jS7Mk5G9jQd2MctWE7X/bsOWXCf3gddtN2bU/24="], meta: []]],
  hash: "OucG7VliTnsNY/ebqO/T65Z6lFd4fBbOEm0nDRvNu9Y=", bond_size: 100000]]
