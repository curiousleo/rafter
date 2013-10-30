-module(bmsup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(M, [Func, N]) ->
    {ok, Super} = supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
    Children = start_children(Super, M, [Func, N, self()], []),
    % Children = [ C || _N <- lists:seq(1, M), {ok, C} <- supervisor:start_child(Super, Args) ],
    receive
        start -> lists:foreach(Children, fun(C) -> C ! latency end),
                 collect_results(Super, M, [])
    end.

collect_results(Super, 0, Results) -> Super ! {results, Results};
collect_results(Super, M, Results) ->
    receive
        {latency, Latency} -> collect_results(Super, M-1, [Latency|Results])
    end.

start_children(_Super, 0, _Args, Children) -> Children;
start_children(Super, M, Args, Children) ->
    {ok, Child} = supervisor:start_child(Super, Args),
    start_children(Super, M-1, Args, [Child|Children]).

init([]) ->
    {ok, {{simple_one_for_one, 5, 60},
          [{bmworker_id,
            {bmworker, start_link, []},
            transient,
            brutal_kill,
            worker,
            [bmworker]}
          ]
         }
    }.
