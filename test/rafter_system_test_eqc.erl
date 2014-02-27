-module(rafter_system_test_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(eqc_statem).

%% eqc_statem exports
-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).

-include("rafter.hrl").
-include("rafter_opts.hrl").

%% This file includes #state{}
-include("rafter_consensus_fsm.hrl").

-compile(export_all).

-type vstruct_spec() :: {[peer()], {atom(), atom()}}.

-record(model_state, {to :: atom(),
                      running = {[], undefined} :: vstruct_spec(),
                      state=init :: init | blank | transitional | stable,
                      oldvstruct :: vstruct_spec(),
                      newvstruct :: vstruct_spec(),
                      commit_index=0 :: non_neg_integer(),
                      last_committed_op :: term(),
                      leader :: atom()}).


-define(QC_OUT(P),
    eqc:on_output(fun(Str, Args) ->
                io:format(user, Str, Args) end, P)).

-define(logdir, "./rafter_logs").

%% ====================================================================
%% Tests
%% ====================================================================

eqc_test_() ->
    {spawn,
     [
      {setup,
       fun setup/0,
       fun cleanup/1,
       [%% Run the quickcheck tests
        {timeout, 120,
         ?_assertEqual(true,
             eqc:quickcheck(
                 ?QC_OUT(eqc:numtests(50, prop_rafter()))))}
       ]
      }
     ]
    }.

%% ====================================================================
%% Helper functions
%% ====================================================================
setup() ->
    os:cmd(["rm -rf ", ?logdir]),
    os:cmd(["mkdir ", ?logdir]),
    {ok, _Started} = application:ensure_all_started(rafter).

cleanup(_) ->
    application:stop(rafter),
    application:stop(lager).

run_prop(Cmds) ->
        aggregate(command_names(Cmds), run_print_cleanup(Cmds)).

run_print_cleanup(Cmds) ->
    {H, S, Res} = run_commands(?MODULE, Cmds),
    eqc_statem:pretty_commands(?MODULE,
        Cmds,
        {H, S, Res},
        cleanup_test(S, Res)).

cleanup_test(#model_state{running={Running, _}}, Res) ->
    [rafter:stop_node(P) || P <- Running],
    os:cmd(["rm -rf ", ?logdir]),
    os:cmd(["mkdir ", ?logdir]),
    ok =:= Res.

start_node(Peer) ->
    Opts = #rafter_opts{logdir=?logdir},
    rafter:start_node(Peer, Opts).

start_nodes({Servers, _}) ->
    [start_node(S) || S <- Servers];
start_nodes(Servers) ->
    [start_node(S) || S <- Servers].

%% ====================================================================
%% EQC Properties
%% ====================================================================

prop_rafter() ->
    ?FORALL(Cmds, more_commands(5, commands(?MODULE)), run_prop(Cmds)).


%% ====================================================================
%% eqc_statem callbacks
%% ====================================================================
initial_state() ->
    #model_state{}.

command(#model_state{state=init}) ->
    {call, ?MODULE, start_nodes, [vstruct_spec()]};

command(#model_state{state=blank, to=To, running={Running, Gen}}) ->
    {call, rafter, set_config, [To, gen_vstruct(Running, Gen)]};

command(#model_state{state=stable, to=To,
                     running={Running, _}, oldvstruct={Old, Gen}}) ->
    YesVotes = dict:from_list([{S, true} || S <- Running]),
    case rafter_voting:quorum(gen_vstruct(Old, Gen), YesVotes) of
        false ->
            NodeToStart = oneof(lists:subtract(Old, Running)),
            {call, rafter, start_node, [NodeToStart]};
        true ->
            frequency([{100, {call, rafter, op, [To, command()]}},
                       {1, {call, rafter, stop_node, [oneof(Running)]}}])
    end.

precondition(#model_state{state=init}, _) ->
    true;
precondition(#model_state{running=undefined},
    {call, rafter, _, _}) ->
        false;
precondition(#model_state{running={Running, _}},
    {call, rafter, _, [To]}) ->
        lists:member(To, Running);
precondition(#model_state{running={Running, _}},
    {call, rafter, op, [To, _]}) ->
        lists:member(To, Running);
precondition(#model_state{running={Running, _}},
    {call, rafter, set_config, [To, _]}) ->
        lists:member(To, Running).

next_state(#model_state{state=init}=S, _,
    {call, ?MODULE, start_nodes, [{Running, Gen}]}) ->
        Leader = lists:nth(1, Running),
        S#model_state{state=blank, running={Running, Gen}, to=Leader,
                      leader=Leader};

%% The initial config is always just the running servers
next_state(#model_state{state=blank, to=To, running={Running, Gen}}=S,
    _Result, {call, rafter, set_config, [To, RunningVstruct]}) ->
        RunningVstruct = gen_vstruct(Running, Gen),
        S#model_state{commit_index=1, state=stable, oldvstruct={Running, Gen}};

next_state(#model_state{state=stable, commit_index=CI, leader=Leader,
  last_committed_op=LastOp}=S, Result, {call, rafter, op, [_, Op]}) ->
    S#model_state{commit_index={call, ?MODULE, maybe_increment, [CI, Result]},
                  leader={call, ?MODULE, maybe_change_leader, [Leader, Result]},
                  last_committed_op={call, ?MODULE, maybe_change_last_op, [LastOp, Op, Result]}};

next_state(#model_state{state=stable, to=To, running={Running, Gen}}=S,
    _Result, {call, rafter, stop_node, [Node]}) ->
        NewRunning = lists:delete(Node, Running),
        case To of
            Node ->
                S#model_state{leader=unknown,
                              running={NewRunning, Gen},
                              to=lists:nth(1, NewRunning)};
            _ ->
                S#model_state{running={NewRunning, Gen}}
        end.


postcondition(#model_state{state=init}, {call, ?MODULE, start_nodes, _}, _) ->
    true;
postcondition(#model_state{state=blank},
  {call, rafter, set_config, [_To, _Servers]}, {ok, _}) ->
    true;

postcondition(#model_state{state=stable, oldvstruct={Old, _}, to=To},
  {call, rafter, op, [To, _]}, {ok, _}) ->
    true =:= lists:member(To, Old);
postcondition(#model_state{state=stable, to=To}, {call, rafter, op, [To, _]},
    {error, {redirect, Leader}}) ->
        Leader =/= To;
postcondition(#model_state{state=stable, to=To}, {call, rafter, op, [To, _]},
    {error, election_in_progress}) ->
        true;

postcondition(#model_state{state=stable},
    {call, rafter, stop_node, [_Node]}, ok) ->
        true.

invariant(ModelState=#model_state{to=To, state=stable}) ->
    {_, State} = sys:get_state(To),
    commit_indexes_monotonic(ModelState, State) andalso
    committed_entry_exists_in_log(ModelState, To);
invariant(_) ->
    true.

%% ====================================================================
%% Helper Functions
%% ====================================================================

maybe_increment(CommitIndex, {ok, _}) ->
    CommitIndex + 1;
maybe_increment(CommitIndex, {error, _}) ->
    CommitIndex.

maybe_change_leader(Leader, {ok, _}) ->
    Leader;
maybe_change_leader(_Leader, {error, {redirect, NewLeader}}) ->
    NewLeader;
maybe_change_leader(_Leader, {error, _}) ->
    unknown.

maybe_change_last_op(_, Op, {ok, _}) ->
    Op;
maybe_change_last_op(CurrentLastOp, _, {error, _}) ->
    CurrentLastOp.

gen_vstruct(Peers, {Mod, Fun}) ->
    apply(Mod, Fun, [Peers]).

%% ====================================================================
%% Invariants
%% ====================================================================

commit_indexes_monotonic(#model_state{commit_index=CI}, State) ->
    State#state.commit_index =< CI.

committed_entry_exists_in_log(#model_state{commit_index=0}, _) ->
    true;
committed_entry_exists_in_log(#model_state{leader=unknown}, _) ->
    true;
committed_entry_exists_in_log(#model_state{commit_index=CI,
                                           last_committed_op=Op}, To) ->
   %% TODO: Make this search backwards for much greater efficiency
   {ok, #rafter_entry{type=Type, index=CommitIndex, cmd=Cmd}} = rafter:get_entry(To, CI),
   case Type of
       config ->
           true;
       op ->
           CommitIndex =:= CI andalso Cmd =:= Op
   end.

%% ====================================================================
%% EQC Generators
%% ====================================================================

%% Commands for a hypothetical backend. Tested with rafter_sm_echo backend.
%% This module is here to test consensus, not the operational capabilities
%% of the backend.
command() ->
    oneof(["inc key val", "get key", "set key val", "keyspace", "config"]).

vstruct_spec() ->
    ?LET({Servers, Gen}, {servers(), rafter_gen:vsgen()}, {Servers, Gen}).

servers() ->
    ?LET(Seq, ?SIZED(Size, resize(0.5, lists:seq(0, Size + 2))),
         shuffle([list_to_atom(integer_to_list(I)) || I <- Seq])).

-endif.
