erlang-timerfd
=====

Erlang port driver for Linux timerfd facility.

I have a soft realtime application that has a task that needs to be executed at a frequency greater than 1KHz. Erlang timers of course don't have this kind of resolution.

I have sucessfully used the Linux timerfd facility in a C++ application to perform this task. My application needs a major rewrite due to new requirements and I am researching the use of Erlang for this effort.

I found no exisiting timerfd library for Erlang on the net. Prehaps I just overlooked it, but in any case here is my version. I am new to Erlang, advice and constructive criticism is welcome.

Build
-----

    $ rebar3 compile

Documentation
-----

    $ rebar3 edoc

Unit Tests
-----

    $ rebar3 eunit


Benchmarking
-----
```
$ rebar3 shell
...
1> timerfd_bench:bench(333, 100000). %interval in microseconds, count
min: 48, avg: 333.41348, median: 333.0 max: 4701, std.dev: 10308.480170663059, total: 100000
99th percentile lo:310  hi:360
```
`./plot/data.dat` is generated in the process of benchmarking.
use `./plot/http-serve.sh` and`$ x-www-browser http://localhost:8000/plot.html` to get a look at disribution graph.
