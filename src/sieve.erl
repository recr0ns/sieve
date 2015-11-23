-module(sieve).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3]).
-export([is_prime/1, ping/0]).

-define(REQUIRED_ARGS, [max_number]).

start_link() ->
  Args = arguments_provider:get(?REQUIRED_ARGS),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_link(Size) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Size], []).

init([SieveSize]) ->
  Sieve = array:new([{size, SieveSize+1}, {fixed, true}, {default, false}]),
  {ok, #{sieve => Sieve, size => 1, max_size => SieveSize}}.

is_prime(Number) ->
  gen_server:call(?MODULE, {is_prime, Number}).

ping() ->
  gen_server:call(?MODULE, ping).

handle_call(ping, _From, State) ->
  {reply, pong, State};

handle_call({is_prime, Number}, _From, State = #{sieve := Sieve, size := Size, max_size := MaxSize}) ->
  case Number of
    N when N<2 ->
      {reply, false, State};
    N when N > MaxSize ->
      {reply, out_of_range, State};
    N when N=<Size ->
      {reply, array:get(N, Sieve), State};
    _ ->
      ExtendedSieve = extend_sieve(Sieve, Size, Number),
      {reply, array:get(Number, ExtendedSieve), #{sieve => ExtendedSieve, size => Number, max_size => MaxSize}}
  end.

extend_sieve(Sieve, OldSize, TargetSize) ->
  case OldSize-TargetSize of
    0 -> Sieve;
    _ ->
      Number = OldSize + 1,
      S = array:set(Number, process(Sieve, Number, 2, trunc(math:sqrt(Number)) + 1), Sieve),
      extend_sieve(S, Number, TargetSize)
  end.

process(_, _Number, N, N) ->
  true;
process(Sieve, Number, Current, Target) ->
  case array:get(Current, Sieve) of
    false ->
      process(Sieve, Number, Current+1, Target);
    true ->
      case Number rem Current of
        0 ->
          false;
        _ -> process(Sieve, Number, Current+1, Target)
      end
  end.
