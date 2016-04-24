: map2 dup nil == if drop r> drop else dup cdr swap car r@ call >r swap r> swap cons swap recurse call then ;
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

: newWeights2 ( Wrong OldWeights PunishmentFraction -- NewWeights )
dup nil == if drop drop else
  2dup car swap car F @ ^ * >r rot r> swap cons tuck cdr swap cdr swap recurse call then;
: newWeights F ! nil tuck newWeights2 call normalize call ;

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

( [integer 1, integer 1]
[ [integer  1, integer 1],
  [integer  0, integer 1],
  [integer  0, integer 2],
  [integer -1, integer 1] ]
wrong_m call  )
: outcomes30 A @ B @ < if integer -1 else integer 1 then ;
: outcomes3 A @ B @ + C @ < if integer 0 else outcomes30 call then ;
: outcomes21 A @ B @ C @ + + D @ < if integer 2 else outcomes3 call then ;
: outcomes20 H @ nil == if outcomes21 call else 
  H @ car integer 1 == if A @ W @ car + A ! else then 
  H @ car integer -1 == if B @ W @ car + B ! else then 
  H @ car integer 0 == if C @ W @ car + C ! else then 
  H @ car integer 2 == if D @ W @ car + D ! else then 
  H @ cdr H ! W @ cdr W ! recurse call then ;
: outcomes2 H ! W ! fraction 0 1 A ! fraction 0 1 B ! fraction 0 1 C ! fraction 0 1 D ! outcomes20 call;

( [integer 1,integer 2,integer 1,integer 0] [fraction 1 1, fraction 1 1, fraction 2 1, fraction 1 2] normalize call swap outcomes2 call )

:outcomes dup car nil == if drop drop r> else
dup hd map call rot dup tuck swap outcomes2 call 
r> cons >r swap tl map call recurse call
then ;
:outcomes0 nil >r outcomes call reverse;
:doit2 F ! 2dup
outcomes0 call Outcomes !
Outcomes @ swap wrong_m call swap F @ newWeights call 
Outcomes @ ;
:doit 
dup tuck
fraction 3 5
doit2 call >r swap
fraction 5 7
doit2 call drop r>;

macro test 
  [fraction 1 1,fraction 1 1,fraction 1 1,fraction 1 1,fraction 1 1] normalize call
[
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer -1,integer -1,integer  1,integer -1,integer 1,integer 2],
  [integer -1,integer -1,integer -1,integer  1,integer 1,integer 2],
  [integer -1,integer  1,integer -1,integer  1,integer 1,integer 2] ]
  doit call
;
