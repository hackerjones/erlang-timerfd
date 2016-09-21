%%%============================================================================
%%% Copyright (c) 2016, Mark Jones <markalanj@gmail.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%============================================================================

%%%============================================================================
%%% @author Mark Jones <markalanj@gmail.com>
%%% @copyright 2016 Mark Jones
%%% @reference http://man7.org/linux/man-pages/man2/timerfd_create.2.html
%%% @version 0.5.0
%%% @doc 
%%% Linux timerfd port driver. 
%%% @end
%%% ===========================================================================
-module(timerfd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-author('Mark Jones <markalanj@gmail.com>').

-define(CREATE, 0).
-define(SETTIME, 1).
-define(GETTIME, 2).
-define(ACK, 3).

%% API exports
-export([
         start/0,
         stop/0,
         create/1,
         close/1,
         set_time/3,
         set_time/2,
         get_time/1,
         ack/1
        ]).

-import(erlang,[monotonic_time/1]).

-type timer() :: port().
-type clockid() :: clock_monotonic | clock_realtime.
-type timespec() :: { Seconds::non_neg_integer(),
                      Nanoseconds::non_neg_integer() }.
-type itimerspec() :: { Interval::timespec(), Initial::timespec() }.

%%=============================================================================
%% API functions
%%=============================================================================

-spec start() -> ok.
% @doc Load the port driver shared library. The create/1 function calls
% this before opening a new port.
% @see create/1

start() ->
    case erl_ddll:try_load("priv/", ?MODULE, [{driver_options, [kill_ports]}])
    of
        {ok, loaded} -> ok;
        {ok, already_loaded} -> ok
    end.

-spec stop() -> ok.
%@doc Unload the port driver shared library. The close/1 function calls
% this after closing a port.
% @see close/1

stop() ->
    case erl_ddll:try_unload(?MODULE, [kill_ports]) of
        {ok, _} -> ok;
        {error, not_loaded} -> ok
    end.

-spec create(ClockId) -> timer() when
      ClockId :: clockid().
% @doc Creates and returns a new timer object port. 
% @see start/0

create(ClockId) when is_atom(ClockId) ->
    case start() of
        ok -> create_timer(ClockId)
    end.

-spec close(Timer) -> ok when
      Timer :: timer().
% @doc Stop and close the timer object port.
% @see stop/0

close(Timer) ->
    case port_close(Timer) of 
        true -> stop(), ok 
    end.

-spec set_time(Timer, NewValue, Absolute) -> {ok, CurrentValue} when
      Timer :: timer(),
      NewValue :: itimerspec() | timespec(),
      Absolute :: boolean(),
      CurrentValue :: itimerspec().
% @doc Arms (starts) or disarms (stops) the timer. Setting NewValue to zeros
% results in disarming the timer. If Absolute is true an absolute timer is
% started. If Absolute is false a relative timer is started. Returns the 
% current setting of the timer like get_time/1. The owning process will 
% receive timeout messages in its mailbox in the form of 
% {timerfd, {timeout, Overrun :: non_neg_integer()}} or
% {timerfd, {error, Reason :: string()}}
% @see set_time/2

set_time(Timer,
         ITimerSpec = {{IntervalSeconds, IntervalNanoseconds},
                       {InitialSeconds, InitialNanoseconds}},
         Absolute)
  when IntervalSeconds > -1, IntervalNanoseconds > -1,
       InitialSeconds > -1, InitialNanoseconds > -1, is_boolean(Absolute) ->
    binary_to_term(port_control(Timer, ?SETTIME,
                                term_to_binary({ITimerSpec, Absolute})));
set_time(Timer, {IntervalSeconds, IntervalNanoseconds}, Absolute) ->
    set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                     {IntervalSeconds, IntervalNanoseconds}}, Absolute).

-spec set_time(Timer, NewValue) -> {ok, CurrentValue} when
      Timer :: timer(),
      NewValue :: itimerspec() | timespec(),
      CurrentValue :: itimerspec().
% @doc This is a conveniance function which operates like set_time/3. However
% this function always starts a relative timer.
% @see set_time/3

set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                 {InitialSeconds, InitialNanoseconds}}) ->
    set_time(Timer, {{IntervalSeconds, IntervalNanoseconds},
                     {InitialSeconds, InitialNanoseconds}}, false);
set_time(Timer, {IntervalSeconds, IntervalNanoseconds}) ->
    set_time(Timer, {{IntervalSeconds,IntervalNanoseconds},
                     {IntervalSeconds,IntervalNanoseconds}}).

-spec get_time(Timer) -> {ok, CurrentValue} when
      Timer :: timer(),
      CurrentValue :: itimerspec().
% @doc Returns the current setting of the timer.

get_time(Timer) ->
    binary_to_term(port_control(Timer, ?GETTIME, term_to_binary([]))).

-spec ack(Timer) -> ok when
      Timer :: timer().
% @doc Acknowledges the last received timeout message. The port driver will 
% not send more timeout messages until the current message is acknowlaged.

ack(Timer) ->
    binary_to_term(port_control(Timer, ?ACK, term_to_binary([]))).

%%=============================================================================
%% Internal functions
%%=============================================================================

-spec create_timer(ClockId) -> timer() when
      ClockId :: clockid().

create_timer(ClockId) ->
    Timer = open_port({spawn, atom_to_list(?MODULE)}, [binary]),
    case binary_to_term(port_control(Timer, ?CREATE, term_to_binary(ClockId)))
    of
        ok -> Timer
    end.

%%=============================================================================
%% Unit tests
%%=============================================================================

-ifdef(EUNIT).

performance_test_loop(State = #{count := Count, 
                                timer := Timer,
                                time := Then,
                                spans := Spans,
                                expirations := Expirations}) when Count > 0 ->
    RxData = receive
                 {Timer, {data, Data}} ->
                     ok = ack(Timer),
                     {monotonic_time(micro_seconds),
                      binary_to_term(Data)}
             after
                 1000 ->
                     throw("timeout waiting for message")
             end,  
    {Now, {timerfd, {timeout, Expiration}}} = RxData,
    Span = Now - Then,
    performance_test_loop(State#{count := Count - 1, time := Now,
                                 spans := [Span|Spans],
                                 expirations := [Expiration|Expirations]}); 
performance_test_loop(State) -> {ok, State}.

performance_test_print_statistics(#{spans := Spans, 
                                    expirations := Expirations}) ->
    F = fun(X, {Len,Sum}) -> {Len+1, Sum+X} end,
    SpanFold = lists:foldl(F, {0,0}, Spans),
    SpanAvg = element(2,SpanFold) / element(1,SpanFold), 
    ExpirationFold = lists:foldl(F, {0,0}, Expirations),
    ExpirationAvg = element(2,ExpirationFold) / element(1,ExpirationFold),
    ?debugFmt("Average ~w microseconds between messages", [SpanAvg]),
    ?debugFmt("Average expirations ~w", [ExpirationAvg]),
    ok.

performance_test() ->
    Timer = create(clock_monotonic),
    ?assertMatch({ok, {{_,_},{_,_}}}, set_time(Timer, {0,500*1000})),
    Result = performance_test_loop(
               #{ timer => Timer, count => 2000, 
                  time => monotonic_time(micro_seconds),
                  spans => [], expirations => []}), 
    ok = close(Timer),
    {ok, State} = Result,
    performance_test_print_statistics(State),
    ok.

create_failure_test() ->
    ?assertError(badarg,create(notaclockid)),
    ?assertError(function_clause,create("notaclockid")).

get_time_test() ->
    Timer = create(clock_monotonic),
    ?assertMatch({ok, {{_,_},{_,_}}}, get_time(Timer)),
    ?assertMatch(ok, close(Timer)).

set_time_test() ->
    Timer = create(clock_monotonic),
    ?assertMatch({ok,{{_,_},{_,_}}}, set_time(Timer, {{1,0},{1,0}}, false)),
    ?assertMatch({ok,{{_,_},{_,_}}}, set_time(Timer, {0,0}, false)),
    ?assertMatch(ok, close(Timer)).

-endif.

