-module(deque).
-include("include/deque.hrl").
-export([from_list/1,to_list/1,
	 empty/0,null/1,
         cons/2,head/1,tail/1,
         snoc/2,last/1,init/1]).

-define(C, 2).

from_list(List) ->
    lists:foldr(fun cons/2, empty(), List).

to_list(#deque{front=F,frontLen=LenF,rear=R,rearLen=LenR}) ->
    {_,Front} = lists:split(length(F) - LenF,F),
    Rear = lists:reverse(lists:sublist(R,LenR)),
    Front ++ Rear.

empty() ->
    #deque{front=[],frontLen=0,
           rear=[],rearLen=0}.

null(#deque{frontLen=0,rearLen=0}) -> true;
null(_Q) -> false.

cons(X,#deque{front=F,frontLen=LenF}=Q) ->
    balance(Q#deque{front=[X|F],frontLen=LenF + 1}).

head(#deque{front=[Head|_Tail]}) -> Head;
head(#deque{rear=[Head|_Tail]}) -> Head.

tail(#deque{front=[]}) -> empty();
tail(#deque{frontLen=LenF}=Q) ->
    balance(Q#deque{frontLen=LenF - 1}).

snoc(Q,X) -> reverse(cons(X,reverse(Q))).

last(Q) -> head(reverse(Q)).

init(Q) -> reverse(tail(reverse(Q))).

reverse(#deque{front=F,frontLen=LenF,rear=R,rearLen=LenR}) ->
    #deque{front=R,frontLen=LenR,rear=F,rearLen=LenF}.

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
