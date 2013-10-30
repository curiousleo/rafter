-module(bmsup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([Func, N]) ->
    {ok, {{simple_one_for_one, 5, 60},
          [{bmworker_id,
            {bmworker, start_link, [Func, N]},
            transient,
            brutal_kill,
            worker,
            [bmworker]}
          ]
         }
    }.
