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

-module(timerfd_tests).

-import(erlang, [monotonic_time/1]).
-include_lib("eunit/include/eunit.hrl").

performance_test_loop(State = #{count := Count, 
                                timer := Timer,
                                time := Then,
                                spans := Spans,
                                expirations := Expirations}) when Count > 0 ->
    RxData = receive
                 {Timer, {data, Data}} ->
                     ok = timerfd:ack(Timer),
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
    Timer = timerfd:create(clock_monotonic),
    ?assertMatch({ok, {{_,_},{_,_}}}, timerfd:set_time(Timer, {0,500*1000})),
    Result = performance_test_loop(
               #{ timer => Timer, count => 2000, 
                  time => monotonic_time(micro_seconds),
                  spans => [], expirations => []}), 
    ok = timerfd:close(Timer),
    {ok, State} = Result,
    performance_test_print_statistics(State),
    ok.

create_failure_test() ->
    ?assertError(badarg,timerfd:create(notaclockid)),
    ?assertError(function_clause,timerfd:create("notaclockid")).

get_time_test() ->
    Timer = timerfd:create(clock_monotonic),
    ?assertMatch({ok, {{_,_},{_,_}}}, timerfd:get_time(Timer)),
    ?assertMatch(ok, timerfd:close(Timer)).

set_time_test() ->
    Timer = timerfd:create(clock_monotonic),
    ?assertMatch({ok,{{_,_},{_,_}}}, timerfd:set_time(Timer, 
                                                      {{1,0},{1,0}}, false)),
    ?assertMatch({ok,{{_,_},{_,_}}}, timerfd:set_time(Timer, {0,0}, false)),
    ?assertMatch(ok, timerfd:close(Timer)).

