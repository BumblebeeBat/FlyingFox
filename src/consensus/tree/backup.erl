-module(backup).
-export([hash/0, backup/0, backup_files/0]).

files() -> [constants:blocks(), constants:block_pointers(), constants:accounts(), constants:all_secrets(), constants:d_accounts(), constants:channels(), constants:d_channels(), constants:entropy()].

backup_files() -> tl(tl(files())).

hash() -> hash(files(), []).
hash([], X) -> hash:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).

backup() -> backup(files()).
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, "backup/"++F),
    backup(T).
		 
