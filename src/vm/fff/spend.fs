( given a list of pubkeys and balances, process a list of spend transactions and output the new balances for the pubkeys )

:reduce2 dup nil == if drop r> drop else 
  dup cdr swap car >r swap r> r@ call swap recurse call then;
:reduce >r reduce2 call;  ( Y List Function -- X )
: sum2 + ;
: sum fraction 0 1 swap sum2 reduce call ; 
: normalize3 dup nil == if drop r> drop else
  dup cdr swap car r@ / >r swap r> swap cons swap recurse call then ;
: normalize2 >r swap normalize3 call ;
: normalize dup sum call nil swap normalize2 call ;

: spend_format integer -10 integer -10 integer -13 integer -10;
(FromId, ToId, Amount, Nonce)
: spend1 integer 1 integer 3 fraction 1 13 integer 1;
: spend2 integer 2 integer 3 fraction 1 11 integer 1;

: give ; ( ids weightings id amount -- new_weightings)
: verify_sig_dummy drop drop true ;
: spend dup dup car swap cdr car verify_sig_dummy call or_die car call dup tuck >r >r fraction -1 1 * give call
; ( ids_list weights_list signed_tx -- new_weights )
: doit dup [0] cdr == if swap drop else 
dup >r car spend call ( r> cdr recurse call )
then
; ( ids_list weights_list signed_txs -- new_weights )

(maybe we should store a list of ids on the stack, and use each id to @ fetch a datastructure telling us their balance and nonce.)

macro test
[ integer 1, integer 2, integer 3 ]
[fraction 1 1, fraction 1 1, fraction 1 1] normalize call
[[spend1, signature1], 
 [spend2, signature2]]
doit call
;