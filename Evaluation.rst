EC2 Benchmarking
================

Results
-------

My aim is to have nine box plots in the Evaluation chapter of my dissertation.

There should be three plots for each

- time to consensus,
- Memcached throughput, and
- Memcached latency,

showing benchmark results for

- a cluster of 7 nodes, without simulated failures,
- a cluster of 96 nodes, without simulated failures,
- a cluster of 96 nodes, with simulated failures

with original Rafter, Majority protocol, Grid protocol, and Tree protocol on the x-axis.

Data gathering
--------------

Each test will require the following steps:

1. Start up EC2 instances using a preconfigured AMI
2. Configure instances -- e.g. load and compile code for Erlang nodes
3. Start followers and leader
4. Start simulated workload -- e.g. `memaslap` on an extra instance
5. Stop workload
6. Collect data
7. Clean up and shut down instances

Data will be collected in two ways: time to consensus is measured by the leader node; Memcached throughput and latency by (potentially a modified version of) `memaslap`.

Simulating failures
-------------------

Allowing the leader to fail would make benchmarking very complicated. Failures of followers, on the other hand, should be much easier to simulate. One option would be for the leader to send special `fail` (and potentially `recover`) messages to followers.

The failure of a follower could be simulated in several ways. One would be for the follower to enter a `failed` state where it drops all incoming messages and sends none. Upon receiving a `recover` message, it would re-enter the standard Rafter finite state machine. This would potentially complicate the already quite complex follower fsm. Another option would be for the follower's consensus_fsm process to exit when it receives a `fail` message. It would then be restarted by its supervisor within a short amount of time -- potentially too short to have any measurable impact on the system.
