-module(rafter_backend_memcached).

-behaviour(rafter_backend).

%% rafter_backend callbacks
-export([init/1, stop/1, read/2, write/2]).

-record(state, {peer :: atom() | {atom(), atom()}}).

-define(TABLE, memcached_table).

init(Peer) ->
    State = stop(#state{peer=Peer}),
    _Tid1 = ets:new(?TABLE, [set, named_table, public]),
    State.

stop(State) ->
    catch ets:delete(?TABLE),
    State.

read({get, Keys}, State) ->
    Val = try
              Values = lists:flatmap(
                         fun (Key) ->
                                 lists:map(
                                   fun({_Key, {Value, Flags, _Exptime}}) ->
                                           {Value, Flags}
                                   end, ets:lookup(?TABLE, Key))
                         end,
                         Keys),
              {ok, Values}
          catch _:E ->
              {error, E}
          end,
     {Val, State}.

write({set, Key, Value, Flags, Exptime}, State) ->
    Val = try
              ets:insert(?TABLE, {Key, {Value, Flags, Exptime}}),
              {ok, Value}
          catch _:E ->
              {error, E}
          end,
    {Val, State};

write({add, Key, Value, Flags, Exptime}, State) ->
    Val = try
              case ets:insert_new(?TABLE, {Key, {Value, Flags, Exptime}}) of
                  true -> {ok, Value};
                  false -> not_stored
              end
          catch _:E ->
              {error, E}
          end,
    {Val, State};

write({replace, Key, Value, Flags, Exptime}, State) ->
    case ets:member(?TABLE, Key) of
        true -> write({set, Key, Value, Flags, Exptime}, State);
        _ -> {not_stored, State}
    end;

write({append, Key, Append}, State) ->
    Val = try
              Value = ets:lookup_element(?TABLE, Key, 1),
              NewValue = <<Value/binary,Append/binary>>,
              true = ets:update_element(?TABLE, Key, {1, NewValue}),
              {ok, NewValue}
          catch
              _:badarg ->
                  not_stored;
              _:E ->
                  {error, E}
          end,
    {Val, State};

write({prepend, Key, Prepend}, State) ->
    Val = try
              Value = ets:lookup_element(?TABLE, Key, 1),
              NewValue = <<Prepend/binary,Value/binary>>,
              true = ets:update_element(?TABLE, Key, {1, NewValue}),
              {ok, NewValue}
          catch
              _:badarg ->
                  not_stored;
              _:E ->
                  {error, E}
          end,
    {Val, State};

write({delete, Key}, State) ->
    Val = try
              ets:lookup_element(?TABLE, Key, 1),
              ets:delete(?TABLE, Key),
              deleted
          catch
              _:badarg ->
                  not_found;
              _:E ->
                  {error, E}
          end,
    {Val, State};

write({incr, Key, Value}, State) ->
    Val = try
              {ok, ets:update_counter(?TABLE, Key, {1, Value})}
          catch
              _:badarg ->
                  not_found;
              _:E ->
                  {error, E}
          end,
    {Val, State};

write({decr, Key, Value}, State) ->
    Val = try
              {ok, ets:update_counter(?TABLE, Key, {1, -Value, 0, 0})}
          catch
              _:badarg ->
                  not_found;
              _:E ->
                  {error, E}
          end,
    {Val, State};

write({touch, Key, Exptime}, State) ->
    Val = try
              ets:update_element(?TABLE, Key, {3, Exptime}),
              touched
          catch
              _:badarg ->
                  not_found;
              _:E ->
                  {error, E}
          end,
    {Val, State}.
