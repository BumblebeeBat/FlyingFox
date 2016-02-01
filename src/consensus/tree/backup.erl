-module(backup).
-export([hash/0, backup/0, backup_files/0, read/2, read_size/1, files/0]).
%file_names() -> ["accounts", "all_secrets", "d_accounts", "channels", "d_channels", "entropy"].
files() -> [constants:blocks(), constants:block_pointers(), constants:accounts(), constants:all_secrets(), constants:d_accounts(), constants:channels(), constants:d_channels(), constants:entropy()].


%word_to_file("blocks") -> constants:blocks();
%word_to_file("block_pointers") -> constants:block_pointers();
%word_to_file("accounts") -> constants:accounts();
%word_to_file("all_secrets") -> constants:all_secrets();
%word_to_file("d_accounts") -> constants:d_accounts();
%word_to_file("channels") -> constants:channels();
%word_to_file("d_channels") -> constants:d_channels();
%word_to_file("entropy") -> constants:entropy();
%word_to_file(X) ->
%    io:fwrite("backup word_to_file can't handle "),
%    io:fwrite(packer:pack(X)),
%    io:fwrite(X),
%    io:fwrite("\n").

backup_files() -> tl(tl(files())).

hash() -> hash(backup_files(), []).
hash([], X) -> hash:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).
-define(backup, "backup/").
backup() -> backup(backup_files()).
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, ?backup++F),
    backup(T).

-define(word, constants:word_size()).
read_size(File) ->
    %File = word_to_file(F),
    filelib:file_size(?backup++File) div ?word.
read(File, N) ->
    %File = word_to_file(F),
    {ok, RFile } = file:open(?backup++File, [read, binary, raw]),
    {ok, Out} = file:pread(RFile, N*?word, ?word),
    file:close(RFile),
    Out.

