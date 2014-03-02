EC2 Benchmarking
================

Results
-------

There should be plots for time to consensus (TTC) and Memcached latency on the y-axis for a number of different cluster sizes and with different failure simulations with original Rafter, Majority protocol, Grid protocol, and Tree protocol on the x-axis.

Interesting (near-optimal) cluster sizes below 20:

- Majority: uneven numbers of nodes
- Grid: 2 * 3 = 6, 3 * 3 = 9, 3 * 4 = 12, 4 * 4 = 16, 4 * 5 = 20
- Tree (d = 2): 3, 7, 15
- Tree (d = 3): 4, 13

Data gathering
--------------

Each test will require the following steps:

1. Start up EC2 instances using a preconfigured AMI -- use `awsfab setup:num=N` or the EC2 Console
2. Name the instances according to their function (memaslap, leader, follower1 - follower(N-1))
3. Tag exactly those instances that should form the test cluster with `Environment = benchmark` using the EC2 Console
4. Deploy and compile the code using `awsfab --ec2tags=Environment=benchmark deploy`
5. Start followers using `awsfab --ec2tags=Environment=benchmark start_followers`
6. Start leader using `awsfab --ec2tags=Environment=benchmark start_leader`
7. Start simulated workload: log into instance `memaslap` using `awsfab -E memaslap ec2_login` and run `memaslap` from there, targeting the IP address of the `leader` instance
8. Collect data: When the `memaslap` run is over, copy its output to a file. Then end the Erlang session started using `awsfab --ec2tags=Environment=benchmark start_leader` by issuing `q().` -- this task will then automatically download the TTC measurements.
9. Kill all Erlang processes and clean up using `awsfab --ec2tags=Environment=benchmark stop_cluster`
10. Examine results

Data will be collected in two ways: time to consensus is measured by the leader node; Memcached throughput and latency by (potentially a modified version of) `memaslap`.

Simulating failures
-------------------

Allowing the leader to fail would make benchmarking very complicated. Failures of followers, on the other hand, are easier to simulate.

The way I implemented failure simulation currently works as follows: When the leader receives a message `{start_failures, Lambda, T}`, it starts sending `{fail, T}` messages to random followers. The time between two `{fail, T}` messages the leader sends is random, governed by an exponential distribution with parameter `Lambda`. When a follower receives a `{fail, T}` message, it goes into a `failed` state where it discards all incoming messages. After `T` milliseconds, the node goes back into the `follower` state.
