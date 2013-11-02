-module(bmworker).

-export([time_rpc/2]).

-define(TIMEOUT, 3000).

repeat_rpc(_Func, 0) -> ok;
repeat_rpc({Node, M, F, A}, N) ->
    rpc:call(Node, M, F, A, ?TIMEOUT),
    repeat_rpc({Node, M, F, A}, N-1).

time_rpc(Func, N) ->
    {Latency, ok} = timer:tc(?MODULE, repeat_rpc, [Func, N]),
    Latency / N.
