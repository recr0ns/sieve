-module(prime_numbers_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
  application:start(crypto),
  %sync:go(),
  prime_numbers_sup:start_link().

stop(_) ->
  %sync:stop(),
  ok.
