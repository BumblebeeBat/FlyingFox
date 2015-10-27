-module(download_blocks).
-export([sync/2]).


sync(IP, Port) ->
    PeerData = talker:talk({height}, IP, Port),
    TheirHeight = PeerData,
    MyHeight = block_tree:height(),
    MH = MyHeight + constants:max_reveal(),
    if
        TheirHeight > MH ->
            fresh_sync(PeerData);
        TheirHeight > MyHeight ->
            get_blocks(MyHeight, TheirHeight, IP, Port);
        true -> 0
    end.

fresh_sync(PeerData) ->
    io:fwrite("fresh sync"),
    0.
    %starting from recent block, walk backward to find the backup hash.
    %download the files, and check that they match the backup hash.
    %load the blocks in from oldest to newest.

get_blocks(Start, Finish, IP, Port) ->
    io:fwrite("get blocks"),
    1.
