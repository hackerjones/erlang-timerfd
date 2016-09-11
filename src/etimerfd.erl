-module(etimerfd).

-author('Mark Jones <markalanj@gmail.com>').

-define(CREATE, 0).
-define(SETTIME, 1).
-define(GETTIME, 2).

%% API exports
-export([
         start/0,
         stop/0,
         create/1,
         close/1,
         set_time/3,
         set_time/2,
         get_time/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    case erl_ddll:try_load("priv/", ?MODULE, [{driver_options, [kill_ports]}])
    of
        {ok, loaded} -> ok;
        {ok, already_loaded} -> ok
    end.

stop() ->
    case erl_ddll:try_unload(?MODULE, [kill_ports]) of
        {ok, _} -> ok;
        {error, not_loaded} -> ok
    end.

create(ClockId) when is_atom(ClockId) ->
    case start() of
        ok -> create_timer(ClockId)
    end.

close(Timer) when is_port(Timer) ->
    case port_close(Timer) of 
        true -> stop(), ok 
    end.

set_time(Timer,
         {{IntervalSeconds, IntervalNanoseconds},
          {InitialSeconds, InitialNanoseconds}},
         Absolute)
  when is_port(Timer), IntervalSeconds > -1, IntervalNanoseconds > -1,
       InitialSeconds > -1, InitialNanoseconds > -1, is_boolean(Absolute) ->
    ITimerSpec = {{IntervalSeconds, IntervalNanoseconds},
                  {InitialSeconds, InitialNanoseconds}},
    binary_to_term(port_control(Timer, ?SETTIME,
                                term_to_binary({ITimerSpec, Absolute})));
set_time(Timer, {IntervalSeconds, IntervalNanoseconds}, Absolute) ->
    set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                     {IntervalSeconds, IntervalNanoseconds}}, Absolute).

set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                 {InitialSeconds, InitialNanoseconds}}) ->
    set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                     {InitialSeconds, InitialNanoseconds}}, false);
set_time(Timer, {IntervalSeconds, IntervalNanoseconds}) ->
    set_time(Timer, {{IntervalSeconds,IntervalNanoseconds},
                     {IntervalSeconds,IntervalNanoseconds}}).


get_time(Timer) when is_port(Timer) ->
    binary_to_term(port_control(Timer, ?GETTIME, term_to_binary([]))).

%%====================================================================
%% Internal functions
%%====================================================================

create_timer(ClockId) ->
    Timer = open_port({spawn, atom_to_list(?MODULE)}, [binary]),
    case binary_to_term(port_control(Timer, ?CREATE, term_to_binary(ClockId)))
    of
        ok -> Timer;
        {error, Result} -> port_close(Timer), {error, Result}
    end.

