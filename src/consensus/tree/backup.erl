-module(backup).
-export([hash/0, backup/0, backup_files/0, read/2, read_size/1, file_names/0]).
file_names() -> ["accounts.db", "all_secrets.db", "d_accounts.db", "channels.db", "d_channels.db", "entropy.db"].
files() -> [constants:blocks(), constants:block_pointers(), constants:accounts(), constants:all_secrets(), constants:d_accounts(), constants:channels(), constants:d_channels(), constants:entropy()].

%I am not using list_to_atom here because it is insecure.
%word_to_file(blocks) -> constants:blocks();
word_to_file("blocks") -> constants:blocks();
%word_to_file(block_pointers) -> constants:block_pointers();
word_to_file("block_pointers") -> constants:block_pointers();
%word_to_file(accounts) -> constants:accounts();
word_to_file("accounts") -> constants:accounts();
%word_to_file(all_secrets) -> constants:all_secrets();
word_to_file("all_secrets") -> constants:all_secrets();
%word_to_file(d_accounts) -> constants:d_accounts();
word_to_file("d_accounts") -> constants:d_accounts();
%word_to_file(channels) -> constants:channels();
word_to_file("channels") -> constants:channels();
%word_to_file(d_channels) -> constants:d_channels();
word_to_file("d_channels") -> constants:d_channels();
%word_to_file(entropy) -> constants:entropy();
word_to_file("entropy") -> constants:entropy();
word_to_file(X) ->
    io:fwrite("backup word_to_file can't handle "),
    io:fwrite(packer:pack(X)),
    io:fwrite(X),
    io:fwrite("\n").

backup_files() -> tl(tl(files())).

hash() -> hash(files(), []).
hash([], X) -> hash:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).
-define(backup, "backup/").
backup() -> backup(files()).
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, ?backup++F),
    backup(T).

-define(word, constants:word_size()).
read_size(F) ->
    File = word_to_file(F),
    1+((filelib:file_size(?backup++File)-?word) div ?word).
read(F, N) ->
    File = word_to_file(F),
    {ok, RFile } = file:open(?backup++File, [read, binary, raw]),
    {ok, Out} = file:pread(RFile, N*?word, ?word),
    file:close(RFile),
    Out.

