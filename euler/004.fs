:m end integer 1000 ;
:m start integer 100 ;
:m flip3 dup integer 100 / swap ( N -- M )
    dup integer 10 rem rot - integer 99 * + ;
:m palindrone dup end / swap ( N -- true/false )
    end rem flip3 == ;
: main 2dup start == swap ( N M --  )
            start == swap and
if
  drop drop Foo @ 
else
  dup start ==
  if
    drop integer 1 - end 
  else
    2dup * Foo @ < 
    if
    else
      2dup * palindrone 
      if
	2dup * Foo ! 
      else
  then then then
  integer 1 - recurse call
then ;
: doit end end main call ;
doit call
