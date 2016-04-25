:add +; 
:reduce2 dup nil == if drop r> drop else 
  dup cdr swap car >r swap r> r@ call swap recurse call then;
:reduce >r reduce2 call; ( Y List Function -- X )

macro test 
  integer 0 [integer 1, integer 2, integer 3, integer 4] add reduce call integer 10 == ;