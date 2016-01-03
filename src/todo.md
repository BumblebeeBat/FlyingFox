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




When I try to sync from too far, I get this error:
The error is probably caused by     Chunk = talker:talk({backup_read, File, 0}, IP, Port),


["signed",["block",0,494,"C7QUd8J5yzyT7HvArj7tRG6k+SFYh29gm+9CtRv0etU=",[-6,["signed",["sign_tx",0,971,"sVNGp39+OE/+IiMT58Of/VoZGc8EqXiwjUdJPbgnEeE=",[-6,196,187,181,179,178,177,168,167,166,162,159,158,156,154,152,150,144,141,140,134,132,130,128,127,126,125,122,117,114,109,108,106,104,101,95,93,90,88,86,85,77,76,74,67,64,61,60,57,56,55,54,53,51,50,47,45,43,34,28,17,11,8,5,4,3,1,0],"C7QUd8J5yzyT7HvArj7tRG6k+SFYh29gm+9CtRv0etU=",493],"TUVVQ0lRQzY0VmcyWU1KS2YrMkFHY1N6aWpsV25hRWN0ZlhuZFRlRnlBZGoyZ1o4U2dJZ09OODVCWElhZ0lJaDVkZ1VraEJ4elQveStvTFRFR3NlMkR4K2pKVjZwWFE9",[-6],[-6]]],1649267662663,"bm9uZQ==",2205011841180,"MTJar5bBCo9t+6l041jGexsL3lqzbyBtjG/RbEKf+k8="],"TUVVQ0lCY25OczZjTVJzbnlUZEJhWlpDSEtia0h6bllQNDFoTld1ZnJQZ0pjWEhDQWlFQWdnVjVONWNYaFhGTmNSa08yV05taTNtYWFaQjR4ZDl3VURYYU41K2hFNUU9",[-6],[-6]]fs 3** exception throw: {error,{invalid_ejson,#Port<0.3100>}}
     in function  jiffy:finish_encode/2 (src/jiffy.erl, line 148)
     in call from talker:talk/2 (src/networking/talker.erl, line 14)
     in call from download_blocks:absorb2/5 (src/consensus/download_blocks.erl, line 39) <--------------- 39 39 39
     in call from download_blocks:absorb_stuff/3 (src/consensus/download_blocks.erl, line 34)
     in call from download_blocks:fresh_sync/3 (src/consensus/download_blocks.erl, line 59)
     in call from download_blocks:sync/2 (src/consensus/download_blocks.erl, line 12)
