-module(dot).

-export([vstruct_dot/1, vstate_dot/1]).

-include("voting.hrl").

-spec vstruct_dot(#vstruct{}) -> ok.
vstruct_dot(#vstruct{tree = Tree, indices = [{Vid1, _}|Indices]}) ->
    io:fwrite("digraph vstruct {~n"),
    io:fwrite("node [shape=Mrecord];~n"),
    %% io:format("indices [label=\"<~p>~p", [Vid1, Vid1]),
    %% lists:foreach(fun({Vid, _}) -> io:format(" | <~p>~p", [Vid, Vid]) end,
    %%               Indices),
    %% io:fwrite("\"];~n"),
    vstruct_dot_rec([], Tree),
    io:fwrite("}~n").

-spec vstruct_dot_rec([non_neg_integer()], #vstruct_p{} | #vstruct_v{}) -> ok.
vstruct_dot_rec(Path, #vstruct_p{votes = V, id = Id}) ->
    io:format("\"~p\" [label=\"~p | ~p\"];~n", [Path, Id, V]);
    %% io:format("indices:\"~p\" -> \"~p\";~n", [Id, Path]);
vstruct_dot_rec(Path, #vstruct_v{votes = V, thresh = T, children = Children}) ->
    io:format("\"~p\" [label=\"V | { ~p | ~p }\"];~n", [Path, V, T]),
    lists:foldl(fun(Child, K) ->
                        ChildPath = Path ++ [K],
                        vstruct_dot_rec(ChildPath, Child),
                        io:format("\"~p\" -> \"~p\";~n", [Path, ChildPath]),
                        K + 1
                end,
                1, Children).

-spec vstate_dot(#vstate{}) -> ok.
vstate_dot(#vstate{tree = Tree}) ->
    io:fwrite("digraph vstate {~n"),
    io:fwrite("node [shape=Mrecord];~n"),
    vstate_dot_rec([], Tree),
    io:fwrite("}~n").

-spec vstate_dot_rec([non_neg_integer()], #vstate_p{} | #vstate_v{}) -> ok.
vstate_dot_rec(Path, #vstate_p{vote = Vote}) ->
    C = case Vote of yes -> "green"; no -> "red"; pending -> "black" end,
    io:format("\"~p\" [label=\"p\", color=~s];~n", [Path, C]);
vstate_dot_rec(Path, #vstate_v{votes = V, yes_votes = YesVotes,
                               no_votes = NoVotes, thresh = T,
                               children = Children}) ->
    C = case {YesVotes >= T, length(Children) < T + NoVotes} of
            {true, _} -> "green";
            {_, true} -> "red";
            _ -> "black"
        end,
    io:format("\"~p\" [label=\"V | { ~p | ~p (~p/~p) }\", color=~s];~n",
              [Path, V, T, YesVotes, NoVotes, C]),
    lists:foldl(fun(Child, K) ->
                        ChildPath = Path ++ [K],
                        vstate_dot_rec(ChildPath, Child),
                        io:format("\"~p\" -> \"~p\";~n", [Path, ChildPath]),
                        K + 1
                end,
                1, Children).
