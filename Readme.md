# Testing

1.  Start the nodes: in four different terminals, run `erl -sname sup`, `erl -sname fib`, `erl -sname w1`, and `erl -sname w2`.

2.  Connect the nodes: connect one of the nodes to all other nodes, e.g.:
    
        (sup@mycomp)1> net_kernel:connect(fib@mycomp).
	true
        (sup@mycomp)2> net_kernel:connect(w1@mycomp).
	true
        (sup@mycomp)3> net_kernel:connect(w2@mycomp).
	true

3.  Compile the required files:
    
        (sup@mycomp)4> c("src/bmsup").
	ok
        (sup@mycomp)5> c("src/bmdistr").
	ok
        (sup@mycomp)6> c("src/bmworker").
	ok

4.  Start the gen_servers:
    
        (w1@mycomp)1> {ok, S} = gen_server:start_link(bmsup, [], []).
	{ok,<0.248.0>}
	(w1@mycomp)2> register(bmsup_p, S).
	true

    ... and the same on w2@mycomp.

5.  Run the benchmark:
    
        (sup@mycomp)7> bmdistr:benchmark([w1@mycomp, w2@mycomp], {fib@mycomp, fib, fib, [35]}, 3, 2).
