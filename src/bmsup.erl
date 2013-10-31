-module(bmsup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([]) ->
    {ok, Super} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    erlang:register(bmsup_p, self()),
    receive
        {Distr, {init, {Conc, Func, Times}}} ->
            Children = start_children(Super, Conc, [Func, Times, self()], [])
            % Children = [ C || _N <- lists:seq(1, Conc), {ok, C} <- supervisor:start_child(Super, Args) ]
    end,
    Distr ! ready,
    receive
        start -> lists:foreach(fun(C) -> C ! latency end, Children)
    end,
    collect_results(Distr, Conc, []).

collect_results(Distr, 0, Results) -> Distr ! {results, Results};
collect_results(Distr, Conc, Results) ->
    receive
        {latency, Latency} -> collect_results(Distr, Conc-1, [Latency|Results])
    end.

start_children(_Super, 0, _Args, Children) -> Children;
start_children(Super, Conc, Args, Children) ->
    {ok, Child} = supervisor:start_child(Super, Args),
    start_children(Super, Conc-1, Args, [Child|Children]).

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
