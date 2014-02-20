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

Allowing the leader to fail would make benchmarking very complicated. Failures of followers, on the other hand, are easier to simulate.

The way I implemented failure simulation currently works as follows: When the leader receives a message `{start_failures, Lambda, T}`, it starts sending `{fail, T}` messages to random followers. The time between two `{fail, T}` messages the leader sends is random, governed by an exponential distribution with parameter `Lambda`. When a follower receives a `{fail, T}` message, it goes into a `failed` state where it discards all incoming messages. After T milliseconds, the node goes back into the `follower` state.
