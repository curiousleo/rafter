-module(bmsup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 60},
          [{bmworker_id,
            {bmworker, start_link, []},
            transient,
            brutal_kill,
            worker,
            [bmworker]}
          ]
         }
    }.
