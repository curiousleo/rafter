-record(vstruct_p, {
          id :: { term(), pid() }, %% should be { Host, Pid }
          parent :: term() %% should be #vstruct_v{}
}).

-record(vstruct_v, {
          votes = 1 :: non_neg_integer,
          thresh :: non_neg_integer,
          parent :: #vstruct_v{} | null,
          children :: [ #vstruct_v{} | #vstruct_p{} ]
}).

-record(vstate_p, {
          vote = pending :: pending | yes | no,
          parent :: term() %% should be #vstate_v{}
}).

-record(vstate_v, {
          yes_votes = 0 :: non_neg_integer,
          no_votes = 0 :: non_neg_integer,
          parent :: #vstate_v{} | null,
          children :: [ #vstate_v{} | #vstate_p{} ]
}).
