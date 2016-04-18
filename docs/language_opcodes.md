opcode_number 
0 hash ( X -- <<Bytes:256>> )
1 verify_sig ( Sig Data Pub -- true/false )
2 + ( X Y -- Z )
3 - ( X Y -- Z )
4 * ( X Y -- Z )
5 / ( X Y -- Z )
6 > ( X Y -- true/false )
7 < ( X Y -- true/false )
8 = ( X Y -- true/false )
9 swap ( A B -- B A )
10 drop ( X -- )
11 dup ( X -- X X )
12 tuck ( a b c -- c a b ) 
13 rot ( a b c -- b c a )
14 2dup ( a b -- a b a b )
15 tuckn ( X N -- ) inserts X N-deeper into stack.
16 pickn ( N -- X )
17 if  conditional statement
18 else  part of an switch conditional statement
19 then part of switch conditional statement.
20 and ( true/false true/false -- true/false )
21 or %( true/false true/false -- true/false )
22 xor %( true/false true/false -- true/false )
23 not %( true/false -- false/true )
24 append %( <<Binary1/binary>> <<Binary2/binary>> -- <<Binary1/binary, Binary2/binary>> )
25 stripr %( <<Binary/binary>> -- <<ShorterBinary/binary>> )
26 stripl %( <<Binary/binary>> -- <<ShorterBinary/binary>> )
27 flip %entire stack is flipped.
28 crash %code stops execution here. Neither person gets the money.
29 f2i %( F -- I ) fraction to integer
30 i2f %( I -- F ) integer to fraction
31 total_coins %( -- TotalCoins )
32 height %( -- Height )
33 stack_size %( -- Size )
34 slash %( -- true/false)
35 == %( X Y -- true/false )
36 : % this starts the function declaration.
37 ; % This symbol ends a function declaration. example : square dup * ;
38 call %Use the binary at the top of the stack to look in our hashtable of defined words. If it exists call the code, otherwise crash.
39 >r %( V -- )
40 r> %( -- V )
41 recurse %crash. this word should only be used in the definition of a word.
42 match ( P X -- true/false )Use the binary to look up a defined word. Make sure the word matches the code that follows 'match', otherwise crash. P is a function, X is an integer.
43 rem % (A B -- C) only works for integers.
44 ! % ( X Y -- )
45 @ ( Y -- X )
46 print % ( Y -- X )
47 gas % ( Y -- X )
true true
false false

These are compiler macros to make it easier to program.

( a open parenthesis starts a multi-line comment block.
) a closed parenthesis ends the comment. Make sure to put spaces between the parenthesis and the other words. 
r@ ( -- X ) copies the top of the r-stack to the main stack.
fraction ( X Y -- F ) makes a fraction from 2 integers. example: fraction 4 6
integer ( X -- Y ) loads an integer. example: "integer 27"
binary ( B -- A ) loads a binary encoded in base64
or_die ( B -- ) if B is true, then does nothing. if B is false, then it crashes.
commit_reveal ( F N -- M ) N is a number that is required to be in the datastructure that you don't have control over. M, the output, is the number you are revealing.
+! ( Number Name -- ) increments the variable Name by Number
double_signed_slash ( Sig1, Sig2, Func1, Func2, Pubkey, Constant -- ) either you prove that someone double signed, or it crashes.