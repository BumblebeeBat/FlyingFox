: main dup integer 0 > if integer 1 - integer 0 swap recurse call else drop then ;

macro test
integer 5 main call
;