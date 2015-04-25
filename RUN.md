This shows the steps for running the elixir flying fox code.

zack@t:~/flying-fox$ ls
_build  config  lib  mix.exs  other  README.md  test
zack@t:~/flying-fox$ cd lib/
zack@t:~/flying-fox/lib$ elixirc *.ex
zack@t:~/flying-fox/lib$ iex
Eshell V6.3  (abort with ^G)
Interactive Elixir (1.0.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> Main.start
#PID<0.76.0>
iex(2)> 

You have to leave this terminal open for flying fox to keep running. Besides this iex prompt, you can talk to flying fox through a TCP API. 



