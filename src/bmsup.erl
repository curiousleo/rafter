-module(bmsup).
-behaviour(gen_fsm).

-export([init/1]).

init(Distr) ->
    {ok, waiting, Distr}.

waiting({start, {Func, N}}, Distr) ->
    Sup = supervisor:start_link(bmworker, {Func, N}),
    lists:foreach(fun(_N) -> supervisor:start_child(Sup, []) end, seq(1, N)),
    {next_state, running, {[], N, Distr}}.

running({done, Latency}, {Latencies, 0, Distr}) ->
    Distr ! {self(), done, [Latency|Latencies]},
    {next_state, waiting, Distr};

running({done, Latency}, {Latencies, N, Distr}) ->
    {next_state, running, {[Latency|Latencies], N-1}}.
