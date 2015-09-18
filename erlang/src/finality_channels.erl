-module(finality_channels).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,write/2,test/0,size/0]).
-define(file, "accounts.db").
-record(channel, {delay = 10, bal1 = 0, bal2 = 0, consensus_flag = false, acc1 = 1, acc2 = 2, creationBlockNumber = 1}).
% delay 10, balance1 48, balance2 48, consensus_flag 1, addressInt1 30, addressInt2 30, creationBlockNumber 38. sums to 205 bits
-define(word, 205).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, N, Val}, D) -> 
    false = N > size(),
    {ok, File} = file:open(?file, [write, read, raw]),
    file:pwrite(File, N*?word, Val),
    file:close(File),
    {noreply, D}.
handle_call({read, N}, _From, D) -> 
    {ok, File } = file:open(?file, [read, binary, raw]),
    {ok, X} = file:pread(File, N*?word, ?word),
    file:close(File),
    {reply, X, D}.
read(N) -> 
    Val = gen_server:call(?MODULE, {read, N}),
    << Delay:10, Bal1:48, Bal2:48, ConsensusFlag:1, AddressInt1:30, AddressInt2:30, CreationBlockNumber:38 >> = Val,
    #channel{delay = Delay, bal1 = Bal1, bal2 = Bal2, consensus_flag = ConsensusFlag, acc1 = AddressInt1, acc2 = AddressInt2, creationBlockNumber = CreationBlockNumber}.
%write(N, Delay, Bal1, Bal2, ConsensusFlag, AddressInt1, AddressInt2, CreationBlockNumber) -> 
write(N, Ch) ->
    Val = << (Ch#channel.delay):10, 
             (Ch#channel.bal1):48, 
             (Ch#channel.bal2):48, 
             (Ch#channel.consensus_flag):1, 
             (Ch#channel.acc1):30, 
             (Ch#channel.acc2):30, 
             (Ch#channel.creationBlockNumber):38 >>,
    %Val = <<Balance:48, Nonce:32, P/binary>>,
    gen_server:cast(?MODULE, {write, N, Val}).
size() -> filelib:file_size(?file) div ?word.
append(Ch) -> write(size(), Ch).
test() -> 
    Ch = #channel{},
    append(Ch),
    success.
