-module(generator).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2, generate/4]).

-define(CHUNK, 100).
-define(TOTAL, 3000).
-define(SECOND, 1000000).
-define(PUSH, "RPUSH").
-define(MIN_NUMBER, 2).
-define(REQUIRED_ARGS, [redis_host, redis_port, numbers_list, max_number]).

start_link() ->
  Args = arguments_provider:get(?REQUIRED_ARGS),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([RedisHost, RedisPort, NumbersList, MaxNumber]) ->
  {ok, C} = eredis:start_link(RedisHost, RedisPort),
  Timer = erlang:send_after(1, self(), start),
  {ok, #{timer => Timer, avg => 0, max_number => MaxNumber, numbers_list => NumbersList, redis => C}}.

handle_info(start, #{timer := OldTimer, avg := OldAvg, max_number := MaxNumber, numbers_list := NumbersList, redis := C}) ->
  erlang:cancel_timer(OldTimer),

  {Time, ok} = timer:tc(?MODULE, generate, [MaxNumber, ?CHUNK, C, NumbersList]),
  Avg = (OldAvg+Time)/2,
  Interval = get_interval(Avg),
  Timer = erlang:send_after(Interval, self(), start),
  {noreply, #{timer => Timer, avg => Avg, max_number => MaxNumber, numbers_list => NumbersList, redis => C}}.

terminate(normal, _) ->
  ok.

get_interval(Avg) ->
  Interval = trunc((?SECOND - Avg * ?TOTAL / ?CHUNK) / (?CHUNK - 1) / 1000),
  case Interval of
    I when I<0 -> 1;
    _ -> Interval
  end.

generate(_, 0, _, _) ->
  ok;
generate(MaxNumber, Count, C, NumbersList) ->
  Num = crypto:rand_uniform(?MIN_NUMBER, MaxNumber),
  eredis:q(C, [?PUSH, NumbersList, Num]),
  generate(MaxNumber, Count-1, C, NumbersList).
