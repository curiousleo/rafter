-module(rafter_memcached_sock).

-behaviour(gen_server).

%% api
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-type normal_cmd() ::
   {set | add | replace | append | prepend,   % command
    binary(),                                 % key
    binary(),                                 % flags
    non_neg_integer(),                        % exptime
    non_neg_integer(),                        % bytes
    boolean()}.                               % noreply?
-type cas_cmd() ::
   {cas,
    binary(),                                 % key
    binary(),                                 % flags
    non_neg_integer(),                        % exptime
    non_neg_integer(),                        % bytes
    binary(),                                 % cas unique
    boolean()}.                               % noreply?
-type cmd() :: normal_cmd() | cas_cmd().

%%
%% api
%%

start_link(ListenSocket, Peer) ->
    gen_server:start_link(?MODULE, [ListenSocket, Peer], []).

%%
%% gen_server callbacks
%%

init([ListenSocket, Peer]) ->
    gen_server:cast(self(), accept),
    {ok, {ListenSocket, Peer}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, {ListenSocket, Peer}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    rafter_memcached_sup:start_socket(Peer),
    send(AcceptSocket, "WHATS UP", []),
    {noreply, {AcceptSocket, Peer}}.

handle_info({tcp, Socket, Str}, {AcceptSocket, Peer}) ->
    send(Socket, "~p", [parse_command(Str)]),
    {noreply, {AcceptSocket, Peer}};
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket}, S) ->
    {stop, normal, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% helper functions
%%

-spec parse_command(binary()) -> {ok, cmd()} | error.
parse_command(CmdBin) ->
    StrippedCmdBin = binary:part(CmdBin, {0, byte_size(CmdBin) - 2}),
    case binary:split(StrippedCmdBin, [<<" ">>], [global]) of
        [<<"set">> | Args] ->       parse_args(set, Args);
        [<<"add">> | Args] ->       parse_args(add, Args);
        [<<"replace">> | Args] ->   parse_args(replace, Args);
        [<<"append">> | Args] ->    parse_args(append, Args);
        [<<"prepend">> | Args] ->   parse_args(prepend, Args);
        [<<"cas">> | Args] ->       parse_args(cas, Args);
        _ ->                        error
    end.

parse_args(cas, [Key, Flags, Exp, Bytes, CasUnique]) ->
    {ok,
     {cas, Key, Flags, binary_to_integer(Exp),
      binary_to_integer(Bytes), CasUnique, false}
    };
parse_args(cas, [Key, Flags, Exp, Bytes, CasUnique, <<"noreply">>]) ->
    {ok,
     {cas, Key, Flags, binary_to_integer(Exp),
      binary_to_integer(Bytes), CasUnique, true}
    };
parse_args(Cmd, [Key, Flags, Exp, Bytes]) ->
    {ok,
     {Cmd, Key, Flags, binary_to_integer(Exp),
      binary_to_integer(Bytes), false}
    };
parse_args(Cmd, [Key, Flags, Exp, Bytes, <<"noreply">>]) ->
    {ok,
     {Cmd, Key, Flags, binary_to_integer(Exp),
      binary_to_integer(Bytes), true}
    };
parse_args(_, _) ->
    error.

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
