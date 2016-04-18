: map2 dup nil == if drop r> drop else dup cdr swap car r@ call >r swap r> swap cons swap recurse call then ;
: map >r nil swap map2 call reverse ; ( List Function -- List )
:merge2 
  2dup nil == swap nil == and if drop drop r> else
  2dup nil == if drop r> ++ else drop
  2dup swap nil == if drop r> swap ++ else drop
  dup cdr swap car >r swap 
  dup cdr swap car r> 
  2dup < if else swap then r> cons >r swap cons recurse call
then then then;

:merge nil >r merge2 call swap drop reverse;

:merge_setup2 nil cons ;
:merge_setup merge_setup2 map call ;
:merge_done dup cdr nil == ;
:doit2 merge_done call if car else 
  dup dup car swap cdr car merge call swap cdr cdr swap nil cons swap ++ recurse call
  then; 
:doit merge_setup call doit2 call;
 

( [integer 3, integer 2, integer 1] [integer 5, integer 2, integer 1] merge call )

[integer 10, integer 2, integer 13, integer 4, integer 5] doit call 