-module(rafter_memcached_sock).

-include("rafter.hrl").

-behaviour(gen_server).

%% api
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
          peer :: peer(),
          socket :: port(),
          next = command :: command | data,
          command :: cmd(),
          data = <<>> :: binary(),
          bytes = 0 :: non_neg_integer()
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
    {ok, #state{socket = ListenSocket, peer = Peer, next = command}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket = ListenSocket, peer = Peer}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    rafter_memcached_sup:start_socket(Peer),
    {noreply, S#state{socket = AcceptSocket}}.

handle_info({tcp, Socket, Str}, S = #state{peer = Peer, next = command}) ->
    case parse_cmd(Str) of
        {ok, Cmd = #retrieval_cmd{}} ->
            Result = execute_read_cmd(Peer, Cmd),
            send(Socket, Cmd, Result),
            {noreply, S};
        {ok, Cmd = #update_cmd{}} ->
            Result = execute_update_cmd(Peer, Cmd),
            send(Socket, Cmd, Result),
            {noreply, S};
        {ok, Cmd} ->
            {noreply, S#state{next = data, command = Cmd, bytes = bytes(Cmd)}};
        error ->
            send(Socket, "ERROR 1"),
            {noreply, S};
        client_error ->
            send(Socket, "CLIENT_ERROR 1"),
            {noreply, S}
    end;
handle_info({tcp, Socket, Str},
            S = #state{peer = Peer, next = data, data = Data, command = Cmd,
                       bytes = Bytes})
  when byte_size(Str) =:= Bytes + 2 ->
    case Str of
        <<NewData:Bytes/binary, "\r\n">> ->
            Result = execute_write_cmd(Peer, Cmd,
                                       <<Data/binary,NewData/binary>>),
            send(Socket, Cmd, Result);
        _ ->
            send(Socket, "ERROR 2")
    end,
    {noreply, S#state{next = command, data = <<>>, bytes = 0}};
handle_info({tcp, _Socket, Str},
            S = #state{next = data, data = Data, bytes = Bytes})
  when byte_size(Str) < Bytes + 2 ->
    {noreply, S#state{data = <<Data/binary,Str/binary>>,
                      bytes = Bytes - byte_size(Str)}};
handle_info({tcp, Socket, _Str}, S = #state{next = data, bytes = Bytes})
  when Bytes < 1 ->
    send(Socket, "ERROR 3"),
    {noreply, S#state{data = <<>>, bytes = 0, next = command}};
handle_info({tcp, Socket, _Str}, S = #state{next = data}) ->
    send(Socket, "ERROR 4"),
    {noreply, S#state{data = <<>>, bytes = 0, next = command}};
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

-spec parse_cmd(binary()) -> {ok, cmd()} | error.
parse_cmd(CmdBin) ->
    StrippedCmdBin = binary:part(CmdBin, {0, byte_size(CmdBin) - 2}),
    case binary:split(StrippedCmdBin, [<<" ">>], [global]) of
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
parse_args(Cmd, [Key]) ->
    {ok, #update_cmd{command = Cmd, key = Key}};
parse_args(Cmd, [Key, Flags, Exp, Bytes]) ->
    {ok, #storage_cmd{command = Cmd, key = Key, flags = Flags,
                      exptime = binary_to_integer(Exp),
                      bytes = binary_to_integer(Bytes), noreply = false}
    };
parse_args(Cmd, [Key, Flags, Exp, Bytes, <<"noreply">>]) ->
    {ok, #storage_cmd{command = Cmd, key = Key, flags = Flags,
                      exptime = binary_to_integer(Exp),
                      bytes = binary_to_integer(Bytes), noreply = false}
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

send(Socket, _Cmd, stored) ->
    send(Socket, "STORED");
send(Socket, _Cmd, not_stored) ->
    send(Socket, "NOT_STORED");
send(Socket, _Cmd, touched) ->
    send(Socket, "TOUCHED");
send(Socket, _Cmd, exists) ->
    send(Socket, "EXISTS");
send(Socket, _Cmd, not_found) ->
    send(Socket, "NOT_FOUND");
send(Socket, _Cmd, deleted) ->
    send(Socket, "DELETED");
send(Socket, #update_cmd{}, {ok, Value}) ->
    send(Socket, Value);
send(Socket, #retrieval_cmd{}, {ok, Values}) ->
    lists:map(
      fun({Key, Value, Flags}) ->
              send(Socket,
                   io_lib:format("VALUE ~s ~s ~B~n~s",
                                 [Key, Flags, byte_size(Value), Value]))
      end, Values).

send(Socket, Str) ->
    ok = gen_tcp:send(Socket, Str ++ "~r~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
