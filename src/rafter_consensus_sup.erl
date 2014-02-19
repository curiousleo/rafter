-module(rafter_consensus_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

start_link(Me, Opts) when is_atom(Me) ->
    SupName = name(Me, "sup"),
    start_link(Me, SupName, Me, Opts);
start_link(Me, Opts) ->
    {Name, _Node} = Me,
    SupName = name(Name, "sup"),
    start_link(Name, SupName, Me, Opts).

init([NameAtom, Me, Opts]) ->
    LogServer = { rafter_log,
                 {rafter_log, start_link, [NameAtom, Opts]},
                 permanent, 5000, worker, [rafter_log]},

    ConsensusFsm = consensus_fsm(NameAtom, Me, Opts),

    {ok, {{one_for_all, 5, 10}, [LogServer, ConsensusFsm]}}.

%% ===================================================================
%% Private Functions
%% ===================================================================
name(Name, Extension) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ Extension).

start_link(NameAtom, SupName, Me, Opts) ->
    process_flag(trap_exit, true),
    supervisor:start_link({local, SupName}, ?MODULE, [NameAtom, Me, Opts]),
    fail_loop(NameAtom, Me, Opts).

fail_loop(NameAtom, Me, Opts) ->
    receive
        {'EXIT', From, {fail, T}} ->
            ok = supervisor:terminate_child(?MODULE, From),
            supervisor:delete_child(?MODULE, From),
            spawn(
              fun () ->
                      io:format("Sleeping for ~p~n", [T]),
                      timer:sleep(T),
                      ConsensusFsm = consensus_fsm(NameAtom, Me, Opts),
                      io:format("Starting new consensus fsm~n", []),
                      supervisor:start_child(?MODULE, ConsensusFsm)
              end),
            fail_loop(NameAtom, Me, Opts)
    end.

consensus_fsm(NameAtom, Me, Opts) ->
    { rafter_consensus_fsm,
      {rafter_consensus_fsm, start_link, [NameAtom, Me, Opts]},
      transient, 5000, worker, [rafter_consensus_fsm]}.
