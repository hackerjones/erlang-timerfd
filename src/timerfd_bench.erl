-module(timerfd_bench).
-export([bench/2]).

perf_loop(State = #{count := Count,
                    timer := Timer,
                    time := Then,
                    span_list := SpanList,
                    expiration_list := ExpirationList}) when Count > 0 ->
    receive
        {Timer, {data, Data}} ->
            case binary_to_term(Data) of
                {timerfd,ready} ->
                    Now = erlang:monotonic_time(micro_seconds),
                    {ok, Expiration} = timerfd:read(Timer),
                    Span = Now - Then,
                    perf_loop(State#{count := Count - 1,
                                     time := Now,
                                     span_list := [Span|SpanList],
                                     expiration_list :=
                                         [Expiration|ExpirationList]})
            end
    after
        1000 ->
            throw("timeout waiting for message")
    end;
perf_loop(State) -> {ok, State}.

perf_print_stats(#{span_list := SpanList,
                   expiration_list := ExpirationList}) ->
    Spans = lists:sort(SpanList),
    Min = hd(Spans),
    Max = lists:last(Spans),
    Sum = lists:sum(Spans),
    Len = length(Spans),
    Avg = Sum/Len,
    Std = math:sqrt(lists:sum(lists:map(fun(X) -> math:pow(X-Avg,2) end, Spans))),

    Mid = Len div 2,
    Rem = Len rem 2,
    Med = (lists:nth(Mid+Rem, Spans) + lists:nth(Mid+1, Spans)) / 2,

    Len99lo = Len div 100,
    Len99hi = Len - Len99lo,
    PercLo = lists:nth(Len99lo, Spans),
    PercHi = lists:nth(Len99hi, Spans),

    file:write_file("./plot/data.dat",
                    lists:map(fun(N) -> [erlang:integer_to_binary(N), $\n] end, Spans)),

    Expirations = length(ExpirationList),
    io:format("min: ~p, avg: ~p, median: ~p max: ~p, std.dev: ~p, total: ~p~n", [Min, Avg, Med, Max, Std, Len]),
    io:format("99th percentile\tlo:~p\thi:~p~n", [PercLo, PercHi]),
    ok.

bench(Interval, Count) ->
    {ok, Timer} = timerfd:create(clock_monotonic),
    {ok, _} = timerfd:set_time(Timer, {0,Interval*1000}),
    Result = perf_loop(
               #{ timer => Timer, count => Count,
                  time => erlang:monotonic_time(micro_seconds),
                  span_list => [], expiration_list => []}),
    ok = timerfd:close(Timer),
    {ok, State} = Result,
    perf_print_stats(State),
    ok.
