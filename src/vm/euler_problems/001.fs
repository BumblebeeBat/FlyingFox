
: f r@ integer 0 == if else
  r@ integer 3 rem integer 0 ==
  r@ integer 5 rem integer 0 ==
  or if r@ + else then
  r> integer 1 - >r recurse call 
then ;
integer 999 >r integer 0 f call