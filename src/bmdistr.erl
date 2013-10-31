-module(bmdistr).

-export([benchmark/4]).

benchmark(Func, Nodes, Conc, Times) ->
    lists:foreach(fun(N) -> {bmsup_p, N} ! {self(), {init, {Conc, Func, Times}}} end, Nodes),
    await_ready(length(Nodes)),
    lists:foreach(fun(N) -> N ! start end, Nodes),
    collect_results(length(Nodes), []).

await_ready(0) -> ok;
await_ready(N) ->
    receive
        ready -> await_ready(N-1)
    end.

collect_results(0, Results) -> Results;
collect_results(N, Results) ->
    receive
        {results, Result} -> collect_results(N-1, [Result|Results])
    end.

