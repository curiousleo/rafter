-module(bmworker).
-behaviour(supervisor).

-export([init/1]).
-export([repeat_rpc/2, time_rpc/3]).

-define(TIMEOUT, 3000).

repeat_rpc(_Func, 0) -> ok;
repeat_rpc({Node, M, F, A}, N) ->
    rpc:call(Node, M, F, A, ?TIMEOUT),
    repeat_rpc({Node, M, F, A}, N-1).

time_rpc(Func, N, Sup) ->
    {Latency, _Value} = timer:tc(?MODULE, repeat_rpc, [Func, N]),
    Sup ! {done, Latency / N}.

init({Func, N, Sup}) ->
    {ok, {{simple_one_for_one, 5, 60},
          [{bmworker_id,
            {?MODULE, time_rpc, [Func, N, Sup]},
            transient,
            brutal_kill,
            worker,
            [bmworker]}
          ]
         }
    }.
