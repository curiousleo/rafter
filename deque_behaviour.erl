-module(deque_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{empty,0},
     {null,1},
     {cons,2},
     {head,1},
     {tail,1},
     {snoc,2},
     {last,1},
     {init,1}
    ];
behaviour_info(_Other) ->
    undefined.
