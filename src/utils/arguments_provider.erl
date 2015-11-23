-module(arguments_provider).

-export([get/1]).

get(Args) ->
  get(Args, []).

get([], Values) ->
  lists:reverse(Values);
get([Arg | Tail], Values) ->
  {ok, Value} = application:get_env(prime_numbers, Arg),
  get(Tail, [Value] ++ Values).
