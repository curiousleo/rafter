-module(rafter_config_eqc).

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

-compile(export_all).

-record(state, {running :: #vstruct{},

                %% #config{} in an easier to match form
                state=blank :: blank | transitional | stable,
                oldvstruct :: #vstruct{},
                newvstruct :: #vstruct{},

                %% The peer we are communicating with during tests
                to :: peer()}).

-define(logdir, "./rafter_logs").

-define(QC_OUT(P),
    eqc:on_output(fun(Str, Args) ->
                io:format(user, Str, Args) end, P)).
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
                 ?QC_OUT(eqc:numtests(50, eqc:conjunction(
                             [{prop_quorum_max,
                                     prop_quorum_max()},
                              {prop_config,
                                  prop_config()},
                              {prop_majority,
                                  prop_majority()}])))))}
       ]
      }
     ]
    }.

setup() ->
    file:make_dir(?logdir),
    {ok, _Started} = application:ensure_all_started(rafter).

cleanup(_) ->
    application:stop(rafter),
    application:stop(lager).

%% ====================================================================
%% EQC Properties
%% ====================================================================

prop_quorum_max() ->
    ?FORALL({Config, {Me, Responses}}, {config(), responses()},
        begin
            ResponsesDict = dict:from_list(Responses),
            case rafter_config:quorum_max(Me, Config, ResponsesDict) of
                0 ->
                    true;
                QuorumMin ->
                    TrueDict = dict:from_list(map_to_true(QuorumMin, Responses)),
                    ?assertEqual(true, rafter_config:quorum(Me, Config, TrueDict)),
                    true
            end
        end).

prop_config() ->
    Opts = #rafter_opts{logdir = ?logdir},
    ?FORALL(Cmds, commands(?MODULE),
        aggregate(command_names(Cmds),
            begin
                Running = [a |
                           [list_to_atom(integer_to_list(I)) ||
                            I <- lists:seq(0, rafter_gen:max_running())]],
                [rafter:start_node(P, Opts) || P <- Running],
                {H, S, Res} = run_commands(?MODULE, Cmds),
                eqc_statem:pretty_commands(?MODULE, Cmds, {H, S, Res},
                    begin
                        [rafter:stop_node(P) || P <- Running],
                        os:cmd(["rm ", ?logdir, "/*.meta"]),
                        os:cmd(["rm ", ?logdir, "/*.log"]),
                        Res =:= ok
                    end)
            end)).

prop_majority() ->
    ?FORALL(
        {Me, Responses}, responses(),
        begin
            %% FIXME: This dict:erase should not be necessary by how
            %% responses() is defined!
            ResponseDict = dict:erase(Me, dict:from_list(Responses)),
            Servers = dict:fetch_keys(ResponseDict),
            Maj = rafter_voting_majority:majority(Servers),
            M1 = {rafter_config_majority:quorum_max(Me, Servers, ResponseDict),
                  rafter_config_majority:quorum(Me, Servers, ResponseDict)},
            M2 = {rafter_config:quorum_max(Me, Maj, ResponseDict),
                  rafter_config:quorum(Me, Maj, ResponseDict)},
            M1 == M2
        end).


%% ====================================================================
%% eqc_statem callbacks
%% ====================================================================

initial_state() ->
    #state{running=rafter_gen:vstruct(a),
           state=blank,
           to=a}.

command(#state{to=To}) ->
    {call, rafter, set_config, [To, rafter_gen:vstruct()]}.

precondition(_, _) ->
    true.

next_state(#state{state=blank}=S,
           _Result, {call, rafter, set_config, [To, Peers]}) ->
    case rafter_voting:member(To, Peers) of
        true ->
            S#state{state=stable, oldvstruct=Peers};
        false ->
            S
    end;

next_state(#state{state=stable, oldvstruct=Old, running=Running}=S,
            _Result, {call, rafter, set_config, [To, Peers]}) ->
    case rafter_voting:member(To, Old) of
        true ->
            Config = #config{state=transitional, oldvstruct=Old, newvstruct=Peers},
            case quorum_impossible(To, Config, Running) of
                true ->
                    S#state{state=transitional, newvstruct=Peers};
                false ->
                    S#state{state=stable, oldvstruct=Peers, newvstruct=undefined}
            end;
        false ->
            %% We can't update config since we already configured ourself
            %% out of the consensus group
            S
    end;

next_state(S, _, _) ->
    S.

%% We have consensus and the contacted peer is a member of the group, but not the leader
postcondition(_S, {call, rafter, set_config, [Peer, _NewServers]}, {error, {redirect, Leader}}) ->
    rafter:get_leader(Peer) =:= Leader;

%% Config set successfully
postcondition(_S, {call, rafter, set_config, _args}, {ok, _newconfig}) ->
    true;

postcondition(#state{running=Running, state=blank},
              {call, rafter, set_config, [Peer, NewServers]}, {error, peers_not_responding}) ->
    Config=#config{state=stable, oldvstruct=NewServers},
    quorum_impossible(Peer, Config, Running);

postcondition(#state{running=Running, oldvstruct=Old, newvstruct=New, state=State},
        {call, rafter, set_config, [Peer, _]}, {error, election_in_progress}) ->
    Config=#config{state=State, oldvstruct=Old, newvstruct=New},
    quorum_impossible(Peer, Config, Running);

postcondition(#state{running=Running, oldvstruct=Old, state=stable},
             {call, rafter, set_config, [Peer, NewServers]}, {error, timeout}) ->
    %% This is the state that the server should have set from this call should
    %% have set before running the reconfig
    C=#config{state=transitional, oldvstruct=Old, newvstruct=NewServers},
    quorum_impossible(Peer, C, Running);

postcondition(#state{oldvstruct=Old, newvstruct=New, state=State},
             {call, rafter, set_config, [Peer, _]}, {error, not_consensus_group_member}) ->
    C=#config{state=State, oldvstruct=Old, newvstruct=New},
    false =:= rafter_config:has_vote(Peer, C);

postcondition(#state{state=State}, {call, rafter, set_config, [_, _]},
              {error, config_in_progress}) ->
    State =:= transitional;

postcondition(#state{running=Running, state=blank},
        {call, rafter, set_config, [Peer, _]}, {error, invalid_initial_config}) ->
    C = #config{state=blank},
    quorum_impossible(Peer, C, Running);

postcondition(#state{oldvstruct=Old},
              {call, rafter, set_config, [_Peer, NewServers]}, {error, not_modified}) ->
    Old =:= NewServers.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec quorum_dict(peer(), list(peer())) -> dict().
quorum_dict(Me, Servers) ->
    dict:from_list([{S, true} || S <- lists:delete(Me, Servers)]).

map_to_true(QuorumMin, Values) ->
    lists:map(fun({Key, Val}) when Val >= QuorumMin ->
                    {Key, true};
                 ({Key, _}) ->
                    {Key, false}
              end, Values).

quorum_impossible(Peer, Config, Running) ->
    Dict = quorum_dict(Peer, rafter_voting:to_list(Running)),
    not rafter_config:quorum(Peer, Config, Dict).

%% ====================================================================
%% EQC Generators
%% ====================================================================

responses() ->
    Me = rafter_gen:server(),
    {Me, list(response(Me))}.

response(Me) ->
    ?SUCHTHAT({Server, _Index},
              {rafter_gen:server(), rafter_gen:non_neg_integer()},
              Me =/= Server).

config() ->
    oneof([stable_config(), blank_config(),
           staging_config(), transitional_config()]).

stable_config() ->
    #config{state=stable,
            oldvstruct=rafter_gen:vstruct()}.

blank_config() ->
    #config{state=blank}.

staging_config() ->
    #config{state=staging,
            oldvstruct=rafter_gen:vstruct(),
            newvstruct=rafter_gen:vstruct()}.

transitional_config() ->
    #config{state=transitional,
            oldvstruct=rafter_gen:vstruct(),
            newvstruct=rafter_gen:vstruct()}.

-endif.
