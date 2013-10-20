-module(deque).
-include("include/deque.hrl").
-export([from_list/1,to_list/1,
	 empty/0,null/1,
         cons/2,head/1,tail/1,
         snoc/2,last/1,init/1]).

-define(C, 2).

-spec from_list(list()) -> #deque{}.
from_list(List) ->
    lists:foldr(fun cons/2, empty(), List).

-spec to_list(#deque{}) -> list().
to_list(#deque{front=F,frontLen=LenF,rear=R,rearLen=LenR}) ->
    {_,Front} = lists:split(length(F) - LenF,F),
    Rear = lists:reverse(lists:sublist(R,LenR)),
    Front ++ Rear.

-spec empty() -> #deque{}.
empty() ->
    #deque{front=[],frontLen=0,
           rear=[],rearLen=0}.

-spec null(#deque{}) -> boolean().
null(#deque{frontLen=0,rearLen=0}) -> true;
null(_Q) -> false.

-spec cons(any(), #deque{}) -> #deque{}.
cons(X,#deque{front=F,frontLen=LenF}=Q) ->
    balance(Q#deque{front=[X|F],frontLen=LenF + 1}).

-spec head(#deque{}) -> any().
head(#deque{front=[Head|_Tail]}) -> Head;
head(#deque{rear=[Head|_Tail]}) -> Head.

-spec tail(#deque{}) -> #deque{}.
tail(#deque{front=[]}) -> empty();
tail(#deque{frontLen=LenF}=Q) ->
    balance(Q#deque{frontLen=LenF - 1}).

-spec snoc(#deque{}, any()) -> #deque{}.
snoc(Q,X) -> reverse(cons(X,reverse(Q))).

-spec last(#deque{}) -> any().
last(Q) -> head(reverse(Q)).

-spec init(#deque{}) -> #deque{}.
init(Q) -> reverse(tail(reverse(Q))).

-spec reverse(#deque{}) -> #deque{}.
reverse(#deque{front=F,frontLen=LenF,rear=R,rearLen=LenR}) ->
    #deque{front=R,frontLen=LenR,rear=F,rearLen=LenF}.

-spec balance(#deque{}) -> #deque{}.
balance(#deque{frontLen=LenF,rearLen=LenR}=Q) when LenF > ?C*LenR + 1 ->
    NewLenF = (LenF + LenR) div 2,
    NewLenR = LenF + LenR - NewLenF,
    {NewF,FrontTail} = lists:split(NewLenF,Q#deque.front),
    NewR = Q#deque.rear ++ lists:reverse(FrontTail),
    #deque{front=NewF,frontLen=NewLenF,rear=NewR,rearLen=NewLenR};
balance(#deque{frontLen=LenF,rearLen=LenR}=Q) when LenR > ?C*LenF + 1 ->
    NewLenF = (LenF + LenR) div 2,
    NewLenR = LenF + LenR - NewLenF,
    {NewR,RearTail} = lists:split(NewLenR,Q#deque.rear),
    NewF = Q#deque.front ++ lists:reverse(RearTail),
    #deque{front=NewF,frontLen=NewLenF,rear=NewR,rearLen=NewLenR};
balance(Q) -> Q.
