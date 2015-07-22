#This thread is like talker in that it is active.
#Watch the mailbox module on several nodes
#delete any messages to me on those nodes, and put the message into the inbox
defmodule CheckMail do
  @name __MODULE__
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: @name])
  end
  def init(_) do
		Task.start_link(fn() -> timer end)
    {:ok, []}
  end
	def doit3(times, p) do
		cond do
			times <= 0 -> []
			true ->
				x = %PopMessage{pub: Keys.pubkey}
				|> Keys.sign
				|> Cli.packer(fn(x) -> 
							IO.puts("checkmail doit3 #{inspect x}")
							out = Cli.talk([:pop, x], p)
							IO.puts("checkmail doit3 #{inspect out}")
							out
						end)
			Task.start(fn() -> doit3(times - 1, p) end)
			IO.puts("check mail #{inspect x}")
			%{msg: x.msg.msg[:msg], key: x.msg.msg[:key]}
			|> Encryption.recieve_msg |> Inbox.record_message
			x.payment |> ChannelManager.accept(0)
		end
	end
	def doit2(peer) do
		fn() ->
			s = %InboxSize{pub: Keys.pubkey}
			|> Keys.sign
			|> Cli.packer(fn(x) -> Cli.talk([:inbox_size, x], peer) end)
			|> doit3(peer)
		end
	end
	def doit do MailNodes.all |> Enum.map(&(Task.start(doit2(&1)))) end

  def timer do
    :timer.sleep(3000)
    doit
    timer
  end
end
