-module(free_constants).
-compile(export_all).
hashlock_time() -> 30.
max_channel() -> constants:initial_coins() div 100000.
    
