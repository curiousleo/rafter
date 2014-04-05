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
          socket :: port()
         }).

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
    {ok, #state{peer = Peer, socket = ListenSocket}}.

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
handle_incoming(<<>>, _Socket, S) ->
    S;
handle_incoming(Str, Socket, S = #state{peer = Peer}) ->
    case Str of
        <<_Magic:8, Opcode:8, KeyLen:16,
          ExtrasLen:8, _DataType:8, _BucketId:16,
          BodyLen:32, Opaque:4/binary, _CAS:64,
          Body:BodyLen/binary, Next/binary>> ->
            case {Opcode, ExtrasLen, Body} of
                {10, 0, <<>>} ->
                    %% NOOP
                    inet:setopts(Socket, [{active, once}]),
                    S;
                {0, 0, <<Key:KeyLen/binary>>} ->
                    %% GET
                    reply(Socket, get, Opaque, execute_get(Peer, Key)),
                    handle_incoming(Next, Socket, S);
                {1, 8, <<Flags:4/binary, Expiry:32, Key:KeyLen/binary,
                         Value/binary>>} ->
                    %% SET
                    Result = execute_set(Peer, {Key, Value, Flags, Expiry}),
                    reply(Socket, set, Opaque, Result),
                    handle_incoming(Next, Socket, S)
            end
    end.

-spec execute_get(peer(), binary()) -> term().
execute_get(Peer, Key) ->
    rafter:read_op(Peer, {get, [Key]}).

-spec execute_set(peer(), {binary(), binary(), binary(), non_neg_integer()}) -> term().
execute_set(Peer, {Key, Value, Flags, Expiry}) ->
    rafter:op(Peer, {set, Key, Value, Flags, Expiry}).

% https://code.google.com/p/memcached/wiki/BinaryProtocolRevamped
reply(Socket, get, Opaque, {ok, [{_Key, Value, Flags}]}) ->
    ValueLen = byte_size(Value),
    ExtrasLen = byte_size(Flags),
    BodyLen = ValueLen + ExtrasLen,
    Resp = <<16#81:8, 0:8, 0:16,
             ExtrasLen:8, 0:8, 0:16,
             BodyLen:32,
             Opaque:4/binary, 0:64,
             Flags/binary,
             Value/binary>>,
    send(Socket, Resp, []);
reply(Socket, get, Opaque, not_found) ->
    Message = <<"Not found">>,
    Resp = <<16#81:8, 0:8, 0:16,
             0:8, 0:8, 1:16,
             9:32,
             Opaque:4/binary, 0:64,
             Message/binary>>,
    send(Socket, Resp, []);
reply(Socket, set, Opaque, stored) ->
    Resp = <<16#81:8, 1:8, 0:16,
             0:8, 0:8, 0:16,
             0:32,
             Opaque:4/binary, 0:64>>,
    send(Socket, Resp, []);
reply(Socket, Cmd, Opaque, _Result) ->
    Opcode = case Cmd of get -> 0; set -> 1 end,
    Resp = <<16#81:8, Opcode:8, 0:16,
             0:8, 0:8, 16#84:16,    % 0x84: internal error
             0:32,
             Opaque:4/binary, 0:64>>,
    send(Socket, Resp, []).

send(Socket, Str, Args) ->
    gen_tcp:send(Socket, [io_lib:format(Str, Args), <<"\r\n">>]),
    inet:setopts(Socket, [{active, once}]),
    ok.
