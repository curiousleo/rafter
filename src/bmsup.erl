-module(bmsup).
-behaviour(gen_server).

-export([start/4]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-record(state, {
    latencies=[] :: list(),
    func :: fun(),
    conc=0 :: non_neg_integer(),
    times=0 :: non_neg_integer(),
    sup :: pid(),
    distr :: pid(),
    children=[] :: list()
}).

start(Func, Conc, Times, Timeout) ->
    {ok, Sup} = gen_server:start_link(?MODULE, {Func, Conc, Times}, []),
    gen_server:call(Sup, start, Timeout).

init({Func, Conc, Times}) ->
    S = #state{func=Func, conc=Conc, times=Times},
    {ok, S}.

handle_call(start, From, S=#state{func=Func, times=Times, conc=Conc}) ->
    {ok, Sup} = supervisor:start_link(bmworker, {Func, Times, self()}),
    Children = lists:map(fun(_N) -> start_child(Sup) end, lists:seq(1, Conc)),
    {noreply, S#state{sup=Sup, distr=From, children=Children}}.

handle_cast({done, Latency}, S=#state{latencies=Latencies, distr=Distr, conc=1}) ->
    io:format("handle_cast received ~w (last one)~n", [{done, Latency}]),
    FinalLatencies = [Latency|Latencies],
    gen_server:reply(Distr, {done, FinalLatencies}),
    {stop, normal, S#state{latencies=FinalLatencies}};

handle_cast({done, Latency}, S=#state{latencies=Latencies, conc=Conc}) ->
    io:format("handle_cast received ~w (remaining ~w)~n", [{done, Latency}, S#state.conc]),
    {noreply, S#state{latencies=[Latency|Latencies], conc=Conc-1}}.

terminate(_Reason, #state{sup=Sup, children=Children}) ->
    lists:foreach(fun(Child) -> kill_child(Sup, Child) end, Children),
    ok.

handle_info(Info, State) ->
    io:format("handle_info received ~w~n", [Info]),
    {stop, {"unknown event", Info, State}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% helper function for waiting/2
start_child(Sup) ->
    {ok, Child} = supervisor:start_child(Sup, []),
    Child.

% helper function for terminate/2
kill_child(Sup, ChildId) ->
    supervisor:terminate_child(Sup, ChildId),
    supervisor:delete_child(Sup, ChildId).
