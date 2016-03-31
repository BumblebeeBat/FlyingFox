FFF

Flying Fox Forth is the turing complete language used for writing contracts on the Flying Fox blockchain. It is only used in channels.

Unlike forth:
this language does not have for loops.
this language does not use the second stack for returns of functions, so you can write a function that leaves more stuff on the 2nd stack than it takes. or a function that takes more stuff than it leaves.
the only state the language uses is the 2 stacks, and a hashtable for functions. It does not have a hashtable for variables.

like forth:
it has 2 stacks.

Flying Fox Forth comes with a compiler. The compiler allows for macros, and uses forth-like syntax.

Here is an example of code using a macro

```
 :m square dup * ; 
 integer 2 square
```

The macro is for squaring numbers. This code applies the macro to a two. Notice the word `integer` in front of the 2. We need to introduce integers to the compiler in this way.

Here is an example of code using a function.

```
 : square dup * ; % multiplies a number by itelf
 integer 2 square call
```

Functions are all referenced by the hash of their contents. Functions are used to build merkle structures, for commit-reveals, etc. This code includes a comment. Everything after the `%` symbol is ignored by the compiler.
This function is for squaring a number. This code applies the function to a two.

Here is an example of code using a recursive function

```
: main dup integer 0 > if integer 1 - integer 0 swap recurse call else drop then ;
integer 5 main call
```

It recursively builds a list of 5 zeros.

Here is code that takes a sha256 of 256 bits, and verifies that it is equal to 256 different bits.

```
binary qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY= hash 
binary 6DIFJeegWoFCARdzPWKgFaMGniG5vD8Qh+WgPZBb5HQ=  ==
```

notice that binary values are encoded in base64, and that they have to be introduced with the word 'binary'.

The first several euler problems are solved in Flying Fox Forth in the /FlyingFox/euler/ folder.