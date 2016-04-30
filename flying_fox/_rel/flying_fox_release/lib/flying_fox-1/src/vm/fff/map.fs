: square dup * ;
: map2 dup nil == if drop r> drop else 
       dup cdr swap car r@ call >r swap r> swap cons swap recurse call then ;
: map >r nil swap map2 call reverse ; ( List Function -- List )

macro test
[integer 5 , integer 6 , integer 7 ] square map call 
[integer 25, integer 36, integer 49] == 
;