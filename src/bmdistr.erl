-module(bmdistr).

-export([benchmark/4]).

benchmark(Func, Nodes, Conc, Times) ->
    lists:foreach(Nodes, fun(N) -> N ! {self(), {init, {Conc, Func, Times}}} end),
    await_ready(),
    lists:foreach(Nodes, fun(N) -> N ! start end),
    collect_results(lists:length(Nodes), []).

collect_results(0, Results) -> Results;
collect_results(N, Results) ->
    receive
        {results, Result} -> collect_results(N-1, [Result|Results])
    end.

