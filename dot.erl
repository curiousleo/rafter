-module(dot).

-export([dot/1]).

-include("voting.hrl").

-spec dot(#vstruct{}) -> ok.

dot(#vstruct{tree = Tree, indices = [{Vid1, _}|Indices]}) ->
    io:fwrite("digraph vstruct {~n"),
    io:fwrite("node [shape=Mrecord];~n"),

    io:format("indices [label=\"<~p>~p", [Vid1, Vid1]),
    lists:foreach(fun({Vid, _}) -> io:format(" | <~p>~p", [Vid, Vid]) end,
                  Indices),
    io:fwrite("\"];~n"),

    dot_rec([], Tree),

    io:fwrite("}").

dot_rec(Path, #vstruct_p{votes = V, id = Id}) ->
    io:format("\"~p\" [label=\"~p | { ~p | 0 }\"];~n", [Path, Id, V]),
    io:format("indices:\"~p\" -> \"~p\";~n", [Id, Path]);

dot_rec(Path, #vstruct_v{votes = V, thresh = T, children = Children}) ->
    io:format("\"~p\" [label=\"V | { ~p | ~p }\"];~n", [Path, V, T]),
    lists:foldl(fun(Child, K) ->
                        ChildPath = Path ++ [K],
                        dot_rec(ChildPath, Child),
                        io:format("\"~p\" -> \"~p\";~n", [Path, ChildPath]),
                        K + 1
                end,
                1, Children).
