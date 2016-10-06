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
%%% @version 0.7.0
%%% @doc 
%%% Linux timerfd port driver. 
%%% @end
%%% ===========================================================================
-module(timerfd).
-author('Mark Jones <markalanj@gmail.com>').

-define(CREATE, 0).
-define(SETTIME, 1).
-define(GETTIME, 2).
-define(READ, 3).

%% API exports
-export([
         start/0,
         stop/0,
         create/1,
         close/1,
         set_time/3,
         set_time/2,
         get_time/1,
         read/1
        ]).

-type timer() :: port().
-type clockid() :: clock_monotonic | clock_realtime.
-type timespec() :: { Seconds::non_neg_integer(),
                      Nanoseconds::non_neg_integer() }.
-type itimerspec() :: { Interval::timespec(), Initial::timespec() }.

%%=============================================================================
%% API functions
%%=============================================================================

% @doc Load the port driver shared library. The create/1 function calls
% this before opening a new port.
% @see create/1
-spec start() -> ok | {error, ErrorDesc} when
      ErrorDesc :: term().

start() ->
    case erl_ddll:load_driver(code:priv_dir(?MODULE), ?MODULE) of 
        ok -> ok;
        {error, ErrorDesc} -> {error, ErrorDesc}
    end.

-spec stop() -> ok | {error, ErrorDesc} when
      ErrorDesc :: term().
%@doc Unload the port driver shared library. The close/1 function calls
% this after closing a port.
% @see close/1

stop() ->
    case erl_ddll:unload_driver(?MODULE) of
        ok -> ok;
        {error, ErrorDesc} -> {error, ErrorDesc}
    end.

-spec create(ClockId) -> {ok, timer()} when
      ClockId :: clockid().
% @doc Creates and returns a new timer object port. 
% @see start/0

create(ClockId) when ClockId == clock_monotonic
                     orelse ClockId == clock_realtime ->
    case start() of
        ok -> open_port_and_create_timer(ClockId);
        Other -> Other
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
% receive ready messages in its mailbox. 
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

-spec read(Timer) -> {ok, Expirations}  | {error, ewouldblock} 
                     | {error, Errno} when
      Timer :: timer(),
      Expirations :: non_neg_integer(),
      Errno :: integer().
% @doc Reads the number of expirations since the last read. The driver
% sends a message to the port owner process when the timer expires. The driver 
% will not send another ready message until the last event is acknowlaged 
% via a read. This prevents overflowing a process mailbox with ready messages.

read(Timer) ->
    binary_to_term(port_control(Timer, ?READ, term_to_binary([]))).

%%=============================================================================
%% Internal functions
%%=============================================================================

-spec open_port_and_create_timer(ClockId) -> timer() when
      ClockId :: clockid().

open_port_and_create_timer(ClockId) ->
    Timer = open_port({spawn, atom_to_list(?MODULE)}, [binary]),
    case binary_to_term(port_control(Timer, ?CREATE, term_to_binary(ClockId)))
    of
        ok -> {ok, Timer};
        Other -> Other
    end.

