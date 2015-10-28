-module(backup).
-export([hash/0, backup/0, backup_files/0]).

files() -> ["blocks.db", "block_pointers.db", "accounts.db", "all_secrets.db", "d_accounts.db", "channels.db", "d_channels.db", "entropy.db"].

backup_files() -> tl(tl(files())).

hash() -> hash(files(), []).
hash([], X) -> hash:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).

backup() -> backup(files()).
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, "backup/"++F),
    backup(T).
		 
