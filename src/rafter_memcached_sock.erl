-module(rafter_memcached_sock).

-include("rafter.hrl").

-behaviour(gen_server).

%% api
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
          next = command :: command | {data, non_neg_integer()},
          peer :: peer(),
          socket :: port(),
          command :: cmd(),
          buffer = <<>> :: binary()
         }).

-record(storage_cmd, {
          command :: set | add | replace | append | prepend,
          key :: binary(),
          flags :: binary(),            % not used by append and prepend
          exptime :: non_neg_integer(), % not used by append and prepend
          bytes :: non_neg_integer(),
          noreply = false :: boolean()
         }).
-record(cas_cmd, {
          key :: binary(),
          flags :: binary(),
          exptime :: non_neg_integer(),
          bytes :: non_neg_integer(),
          cas_unique :: binary(),
          noreply = false :: boolean()
         }).
-record(update_cmd, {
          command :: incr | decr | touch,
          key :: binary()
         }).
-record(retrieval_cmd, {
          keys :: [binary()]
         }).
-type cmd() :: #storage_cmd{} | #cas_cmd{} | #update_cmd{} | #retrieval_cmd{}.

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
    {ok, #state{next = command, peer = Peer, socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket = ListenSocket, peer = Peer}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    rafter_memcached_sup:start_socket(Peer),
    {noreply, S#state{socket = AcceptSocket}}.

handle_info({tcp, Socket, Str}, S) ->
    {noreply, handle_incoming(Str, Socket, S)};
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

-spec handle_incoming(binary(), port(), #state{}) -> #state{}.
handle_incoming(Str, Socket,
                S = #state{next = command, peer = Peer, buffer = Buffer}) ->
    case binary:split(<<Buffer/binary,Str/binary>>, [<<"\r\n">>], []) of
        [CmdStr, Rest] ->
            NewState = S#state{buffer = <<>>},
            case parse_cmd(CmdStr) of
                {ok, Cmd = #retrieval_cmd{}} ->
                    Result = execute_read_cmd(Peer, Cmd),
                    reply(Socket, Cmd, Result),
                    handle_incoming(Rest, Socket, NewState);
                {ok, Cmd = #update_cmd{}} ->
                    Result = execute_update_cmd(Peer, Cmd),
                    reply(Socket, Cmd, Result),
                    handle_incoming(Rest, Socket, NewState);
                {ok, Cmd} ->
                    %% no reply, so set socket active manually
                    ok = inet:setopts(Socket, [{active, once}]),
                    handle_incoming(Rest, Socket,
                                    NewState#state{next = {data, bytes(Cmd)},
                                                   command = Cmd});
                error ->
                    send(Socket, "ERROR 1", []),
                    handle_incoming(Rest, Socket, NewState);
                client_error ->
                    send(Socket, "CLIENT_ERROR 1", []),
                    handle_incoming(Rest, Socket, NewState)
            end;
        [Part] ->
            ok = inet:setopts(Socket, [{active, once}]),
            S#state{buffer = Part}
    end;
handle_incoming(Str, Socket,
                S = #state{next = {data, Bytes}, peer = Peer, command = Cmd,
                           buffer = Buffer}) ->
    NewState = S#state{next = command, buffer = <<>>},
    WithNewlines = Bytes + 2,
    case <<Buffer/binary, Str/binary>> of
        <<Data:Bytes/binary, "\r\n", Rest/binary>> ->
            Result = execute_write_cmd(Peer, Cmd, Data),
            reply(Socket, Cmd, Result),
            handle_incoming(Rest, Socket, NewState);
        <<_Data:WithNewlines/binary, Rest/binary>> ->
            %% malformed input: data is not followed by \r\n
            %% TODO: better recovery
            send(Socket, "CLIENT_ERROR 2", []),
            handle_incoming(Rest, Socket, NewState);
        Part ->
            ok = inet:setopts(Socket, [{active, once}]),
            S#state{buffer = Part}
    end.

-spec parse_cmd(binary()) -> {ok, cmd()} | error.
parse_cmd(CmdStr) ->
    case binary:split(CmdStr, [<<" ">>], [global]) of
        [<<"set">> | Args] ->       parse_args(set, Args);
        [<<"add">> | Args] ->       parse_args(add, Args);
        [<<"replace">> | Args] ->   parse_args(replace, Args);
        [<<"append">> | Args] ->    parse_args(append, Args);
        [<<"prepend">> | Args] ->   parse_args(prepend, Args);
        [<<"cas">> | Args] ->       parse_args(cas, Args);
        [<<"get">> | Keys] ->       parse_args(get, Keys);
        [<<"gets">> | Keys] ->      parse_args(get, Keys);
        [<<"incr">> | Keys] ->      parse_args(incr, Keys);
        [<<"decr">> | Keys] ->      parse_args(decr, Keys);
        [<<"touch">> | Keys] ->     parse_args(touch, Keys);
        _ ->                        error
    end.

parse_args(append, [Key, Bytes]) ->
    {ok, #storage_cmd{command = append, key = Key, bytes = Bytes,
                      noreply = false}};
parse_args(append, [Key, Bytes, <<"noreply">>]) ->
    {ok, #storage_cmd{command = append, key = Key, bytes = Bytes,
                      noreply = true}};
parse_args(prepend, [Key, Bytes]) ->
    {ok, #storage_cmd{command = prepend, key = Key, bytes = Bytes,
                      noreply = false}};
parse_args(prepend, [Key, Bytes, <<"noreply">>]) ->
    {ok, #storage_cmd{command = prepend, key = Key, bytes = Bytes,
                      noreply = true}};
parse_args(cas, [Key, Flags, Exp, Bytes, CasUnique]) ->
    {ok, #cas_cmd{key = Key, flags = Flags, exptime = binary_to_integer(Exp),
                  bytes = binary_to_integer(Bytes), cas_unique = CasUnique,
                  noreply = false}
    };
parse_args(cas, [Key, Flags, Exp, Bytes, CasUnique, <<"noreply">>]) ->
    {ok, #cas_cmd{key = Key, flags = Flags, exptime = binary_to_integer(Exp),
                  bytes = binary_to_integer(Bytes), cas_unique = CasUnique,
                  noreply = true}
    };
parse_args(get, Keys) ->
    {ok, #retrieval_cmd{keys = Keys}};
parse_args(incr, [Key]) ->
    {ok, #update_cmd{command = incr, key = Key}};
parse_args(decr, [Key]) ->
    {ok, #update_cmd{command = decr, key = Key}};
parse_args(touch, [Key]) ->
    {ok, #update_cmd{command = touch, key = Key}};
parse_args(Cmd, [Key, Flags, Exp, Bytes]) ->
    {ok, #storage_cmd{command = Cmd, key = Key, flags = Flags,
                      exptime = binary_to_integer(Exp),
                      bytes = binary_to_integer(Bytes), noreply = false}
    };
parse_args(Cmd, [Key, Flags, Exp, Bytes, <<"noreply">>]) ->
    {ok, #storage_cmd{command = Cmd, key = Key, flags = Flags,
                      exptime = binary_to_integer(Exp),
                      bytes = binary_to_integer(Bytes), noreply = true}
    };
parse_args(_, _) ->
    client_error.

-spec execute_read_cmd(peer(), #retrieval_cmd{}) -> term().
execute_read_cmd(Peer, #retrieval_cmd{keys = Keys}) ->
    rafter:read_op(Peer, {get, Keys}).

-spec execute_update_cmd(peer(), #update_cmd{}) -> term().
execute_update_cmd(Peer, #update_cmd{command = Cmd, key = Key}) ->
    rafter:op(Peer, {Cmd, Key}).

-spec execute_write_cmd(peer(), #cas_cmd{} | #storage_cmd{}, binary()) ->
        {ok, term()} | {error, term()}.
execute_write_cmd(Peer, #storage_cmd{command = Cmd, key = Key,
                                     flags = Flags, exptime = Exp},
                  Value) ->
    rafter:op(Peer, {Cmd, Key, Value, Flags, Exp});
execute_write_cmd(Peer, #cas_cmd{key = Key, flags = Flags,
                                 cas_unique = CasUnique, exptime = Exp},
                  Value) ->
    rafter:op(Peer, {cas, Key, Value, Flags, Exp, CasUnique}).

-spec bytes(#storage_cmd{} | #cas_cmd{}) -> non_neg_integer().
bytes(#storage_cmd{bytes = Bytes}) ->
    Bytes;
bytes(#cas_cmd{bytes = Bytes}) ->
    Bytes.

reply(Socket, _Cmd, stored) ->
    send(Socket, "STORED", []);
reply(Socket, _Cmd, not_stored) ->
    send(Socket, "NOT_STORED", []);
reply(Socket, _Cmd, touched) ->
    send(Socket, "TOUCHED", []);
reply(Socket, _Cmd, exists) ->
    send(Socket, "EXISTS", []);
reply(Socket, _Cmd, not_found) ->
    send(Socket, "NOT_FOUND", []);
reply(Socket, _Cmd, deleted) ->
    send(Socket, "DELETED", []);
reply(Socket, #update_cmd{}, {ok, Value}) ->
    send(Socket, "~s", [Value]);
reply(Socket, #retrieval_cmd{}, {ok, Values}) ->
    lists:map(
      fun({Key, Value, Flags}) ->
              Str = io_lib:format("VALUE ~s ~s ~B~n~s~n",
                                  [Key, Flags, byte_size(Value), Value]),
              ok = gen_tcp:send(Socket, Str)
      end, Values),
    send(Socket, "END", []);
reply(Socket, _Cmd, {error, E}) ->
    send(Socket, "ERROR ~p", [E]).

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
