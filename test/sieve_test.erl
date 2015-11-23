-module(sieve_test).

-include_lib("eunit/include/eunit.hrl").

sieve_start_test() ->
  {State, _Pid} = sieve:start_link(),
  ?assertEqual(State, ok),
  Ping = sieve:ping(),
  ?assertEqual(Ping, pong).

is_prime_test() ->
  sieve:start_link(),
  ?assertEqual(true, sieve:is_prime(2)),
  ?assertEqual(true, sieve:is_prime(3)),
  ?assertEqual(true, sieve:is_prime(5)),
  ?assertEqual(true, sieve:is_prime(11)),
  ?assertEqual(true, sieve:is_prime(19)),
  ?assertEqual(true, sieve:is_prime(31)),
  ?assertEqual(false, sieve:is_prime(4)),
  ?assertEqual(false, sieve:is_prime(10)),
  ?assertEqual(false, sieve:is_prime(15)),
  ?assertEqual(false, sieve:is_prime(21)),
  ?assertEqual(false, sieve:is_prime(30)),
  ?assertEqual(false, sieve:is_prime(77)),
  ?assertEqual(out_of_range, sieve:is_prime(101)).
