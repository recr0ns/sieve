-module(processor).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

-define(POP, "LPOP").
-define(ADD, "SADD").
-define(REQUIRED_ARGS, [redis_host, redis_port, numbers_list, prime_numbers_set]).

start_link() ->
  Args = arguments_provider:get(?REQUIRED_ARGS),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([RedisHost, RedisPort, NumbersList, PrimeNumbersSet]) ->
  {ok, C} = eredis:start_link(RedisHost, RedisPort),
  self() ! start,
  {ok, #{redis => C, numbers_list => NumbersList, prime_numbers_set => PrimeNumbersSet}}.

handle_info(start, State = #{redis := C, numbers_list := NumbersList, prime_numbers_set := PrimeNumbersSet}) ->
  case eredis:q(C, [?POP, NumbersList]) of
    {ok, undefined} ->
      ok;
    {ok, Item} ->
      Num = list_to_integer(binary_to_list(Item)),
      process_number(Num, C, PrimeNumbersSet)
  end,
  self() ! start,
  {noreply, State}.

process_number(Num, C, PrimeNumbersSet) ->
  case sieve:is_prime(Num) of
    true ->
      eredis:q(C, [?ADD, PrimeNumbersSet, Num]);
    _ -> ok
  end.

terminate(normal, _) ->
  ok.
