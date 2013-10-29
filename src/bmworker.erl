-module(bmworker).
-behaviour(gen_server).

-export([repeat_rpc/2]).
-export([latency/3]).
-export([start_link/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 3000).

latency(Pid, F, N) ->
    gen_server:call(Pid, {latency, F, N}).

repeat_rpc(_F, 0) -> ok;
repeat_rpc({Node, M, F, A}, N) ->
    rpc:call(Node, M, F, A, ?TIMEOUT),
    repeat_rpc({Node, M, F, A}, N-1).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, []}.

handle_call({latency, F, N}, _From, _State) ->
    {Latency, ok} = timer:tc(?MODULE, repeat_rpc, [F, N]),
    {reply, Latency / N, []}.

handle_cast(_, _) -> erlang:error("handle_cast").

handle_info(_, _) -> erlang:error("handle_info").

terminate(_, _) -> ok.

code_change(_, _, _) -> erlang:error("code_change").
