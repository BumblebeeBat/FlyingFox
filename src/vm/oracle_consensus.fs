: map2 dup nil == if drop r> drop else dup cdr swap car r@ call >r swap r> cons swap recurse call then ;
: map >r nil swap map2 call reverse ; ( List Function -- List )
:reduce2 dup nil == if drop r> drop else 
  dup cdr swap car >r swap r> r@ call swap recurse call then;
:reduce >r reduce2 call;  ( Y List Function -- X )

: sum2 + ;
: sum fraction 0 1 swap sum2 reduce call ; 
: normalize3 dup nil == if drop r> drop else
  dup cdr swap car r@ / >r swap r> swap cons swap recurse call then ;
: normalize2 >r swap normalize3 call ;
: normalize dup sum call nil swap normalize2 call ;
: hd car ;
: tl cdr ;

: newWeights2 
dup nil == if drop drop else
  2dup car swap car F @ ^ * >r rot r> swap cons tuck cdr swap cdr swap print recurse call then;
: newWeights F ! nil tuck newWeights2 call;

( [integer 2, integer 1, integer 1, integer 0] [fraction 1 1, fraction 1 1, fraction 1 1, fraction 1 1] normalize call fraction 1 2 newWeights call normalize call )

macro whelper == and if drop drop r> integer 1 + >r else;
:wrong2
dup nil == if drop drop r> else
2dup car swap car
2dup integer 1 == swap integer 0 whelper
2dup integer 0 == swap integer 1 whelper
2dup integer -1 == swap integer 0 whelper
2dup integer 0 == swap integer -1 whelper 
     == not if r> integer 2 + >r else 
then then then then then
cdr swap cdr recurse call then;

:wrong integer 0 >r wrong2 call;

([integer -1, integer 1, integer -1, integer 0]
[integer 1, integer 0, integer -1, integer 2]
wrong call )

:wrong_m2 dup nil == if drop drop r> else
2dup car wrong call r> cons >r cdr recurse call then ;
:wrong_m nil >r wrong_m2 call reverse;

[integer 1, integer 1]
[ [integer  1, integer 1],
  [integer  0, integer 1],
  [integer  0, integer 2],
  [integer -1, integer 1] ]
wrong_m call 