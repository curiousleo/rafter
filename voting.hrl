-type vid() :: { term(), pid() }. %% should be { Host, Pid }

-type index() :: { vid(), [ [ non_neg_integer() ] ] }.

-type vote() :: pending | yes | no.

-record(vstruct_p, {
          id :: vid(),
          parent :: term() %% should be #vstruct_v{}
}).

-record(vstruct_v, {
          votes = 1 :: non_neg_integer(),
          thresh :: non_neg_integer(),
          parent :: #vstruct_v{},
          children :: [ #vstruct_v{} | #vstruct_p{} ]
}).

-record(vstruct, {
          tree :: #vstruct_v{},
          indices :: [ index() ]
}).

-record(vstate_p, {
          vote = pending :: vote(),
          parent :: term() %% should be #vstate_v{}
}).

-record(vstate_v, {
          yes_votes = 0 :: non_neg_integer(),
          no_votes = 0 :: non_neg_integer(),
          thresh :: non_neg_integer(),
          parent :: #vstate_v{},
          children :: [ #vstate_v{} | #vstate_p{} ]
}).

-record(vstate, {
          tree :: #vstate_v{},
          indices :: [ index() ]
}).
