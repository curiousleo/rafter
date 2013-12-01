-record(vstruct, {
          id :: { host(), pid() },
          votes = 1 :: non_neg_integer,
          thresh :: non_neg_integer,
          parent :: vstruct | null,
          children :: [ #vstruct{} ]
}).

-record(vstate_v, {
          yes_votes = 0 :: non_neg_integer,
          no_votes = 0 :: non_neg_integer,
          parent :: #vstate_v{} | null,
          children :: [ #vstate_v{} | #vstate_p{} ]
}).

-record(vstate_p, {
          vote :: pending | yes | no
}).
