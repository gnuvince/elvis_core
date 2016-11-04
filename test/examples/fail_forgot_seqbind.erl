-module(fail_forgot_seqbind).

-compile({parse_transform, seqbind}).

-export([abs1/1, abs2/1, abs3/1, abs4/1]).

%% seqbinds everywhere
abs1(X@) when X@ < 0          -> X@ * -1;
abs1(X@) when X@ == 0; X@ > 0 -> X@.


%% seqbind forgotten in guards
abs2(X@) when X@ < 0 -> X@ * - 1;
abs2(X@) when X == 0 -> X@;
abs2(X@) when X > 0  -> X@.


%% seqbind forgotten in multiple guards
abs3(X@) when X == 0; X > 0 -> X@;
abs3(X@)                    -> X@ * -1.


%% seqbind forgotten in expression body
abs4(X@) when X@ < 0 -> X * -1;
abs4(X@)             -> X.
