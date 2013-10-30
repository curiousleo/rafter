-module(bmworker).
-behaviour(gen_server).

-export([repeat_rpc/2]).
-export([latency/1]).
-export([start_link/3]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 3000).

latency(Pid) ->
    gen_server:call(Pid, latency).

repeat_rpc(_Func, 0) -> ok;
repeat_rpc({Node, M, F, A}, N) ->
    rpc:call(Node, M, F, A, ?TIMEOUT),
    repeat_rpc({Node, M, F, A}, N-1).

start_link(Func, N, Pid) -> gen_server:start_link(?MODULE, [Func, N, Pid], []).

init([Func, N, Pid]) -> {ok, {Func, N, Pid}}.

% init([Func={Node, M, F, A}, N]) ->
%     case rpc:call(Node, M, F, A, ?TIMEOUT) of
%         {badrpc, Reason} -> {stop, Reason};
%         _ -> {ok, {Func, N}}
%     end.

handle_call(latency, _From, {Func, N, Pid}) ->
    {Latency, ok} = timer:tc(?MODULE, repeat_rpc, [Func, N]),
    {reply, Latency / N, {Func, N}}.

handle_cast(latency, {Func, N, Pid}) ->
    {Latency, ok} = timer:tc(?MODULE, repeat_rpc, [Func, N]),
    Pid ! {latency, Latency / N},
    {noreply, {Func, N}}.

handle_info(_, _) -> erlang:error("handle_info").

terminate(_, _) -> ok.

code_change(_, _, _) -> erlang:error("code_change").