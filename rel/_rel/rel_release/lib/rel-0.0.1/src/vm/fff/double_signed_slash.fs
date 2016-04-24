:crf integer -10 integer -10; ( -- -10 -10 )
macro commit_reveal swap dup crf match or_die call rot == or_die ; ( Function Number -- )

macro double_signed_slash ( sig1 sig2 Func1 Func2 Pub Constant -- )
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die;
