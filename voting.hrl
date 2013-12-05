-type vid() :: { term(), pid() }. %% should be { Host, Pid }

-type vote() :: pending | yes | no.

-record(vstruct_p, {
          id :: vid(),
}).

-record(vstruct_v, {
          votes = 1 :: non_neg_integer(),
          thresh :: non_neg_integer(),
          parent :: #vstruct_v{},
          children :: [ #vstruct_v{} | #vstruct_p{} ]
}).

-record(vstate_p, {
          vote = pending :: vote(),
          parent :: term() %% should be #vstate_v{}
}).

-type index() :: { vid(), [ #vstate_p{} ] }.

-record(vstate_v, {
          yes_votes = 0 :: non_neg_integer(),
          no_votes = 0 :: non_neg_integer(),
          thresh :: non_neg_integer(),
          parent :: #vstate_v{},
          children :: [ #vstate_v{} | #vstate_p{} ]
}).

-record(vstate, {
          state :: #vstate_v{} | #vstate_p{},
          phys :: [ index() ]
}).
