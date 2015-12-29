looking at backup.erl
working to allow syncing from further behind than finality.
looking at download_blocks.erl


hashlock lightning payments javascript
close_channel javascript multiple steps
change server javascript
delete_account javascript
button to rotate through your inbox

Make an OTP erts package so that it is easier to install.

To make hashlocked tx with scripts more affordable, we should let bet reveals reference blocks that were revealed since max_reveal. That way you don't have to reveal the same data onto the chain twice for a 2 step hashlocked lightning.

are we using arbitrage yet?

update download_blocks so that we can reach consensus with a node that is further in the future than finality.

handler should have every input and output be encrypted. Otherwise eavesdroppers will publish our channel before we want it published.

update now() 

Language should have 3 outputs: portion of money that gets deleted, portion that gets spent, and nonce.



consensus/db.erl:    File = "database.db",
consensus/entropy.erl:-define(LOC, "entropy.db").
consensus/finality/accounts.erl:-define(file, "accounts.db").
consensus/finality/accounts.erl:-define(empty, "d_accounts.db").
consensus/finality/all_secrets.erl:-define(LOC, "all_secrets.db").
consensus/finality/block_finality/block_dump.erl:-define(file, "blocks.db").
consensus/finality/block_finality/block_dump.erl:    T = "temp.db",
consensus/finality/block_finality/block_pointers.erl:-define(file, "block_pointers.db").
consensus/finality/block_finality/block_pointers.erl:-define(start, "pointers_start.db").
consensus/finality/block_finality/block_pointers.erl:    T = "temp.db",
consensus/finality/channels.erl:-define(file, "channels.db").
consensus/finality/channels.erl:-define(empty, "d_channels.db").
consensus/keys.erl:-define(LOC(), "keys.db").
consensus/secrets.erl:-define(LOC, "secrets.db").
consensus/tree/backup.erl:files() -> ["blocks.db", "block_pointers.db", "accounts.db", "all_secrets.db", "d_accounts.db", "channels.db", "d_channels.db", "entropy.db"].
networking/handler.erl:    {ok, filelib:file_size("backup/accounts.db") div ?WORD};
networking/handler.erl:    {ok, File} = file:open("backup/accounts.db", [read, binary, raw]),