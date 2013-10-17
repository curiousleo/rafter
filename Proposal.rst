Extending Raft with Structured Voting
=====================================

:Project Supervisors: Malte Schwarzkopf and Ionel Gog
:Director of Studies: Jonathan Hayman
:Overseers: Markus Kuhn and Neal Lathia

Introduction and Description of Work
------------------------------------

Finding a consensus is a key functionality of any strongly consistent distributed system. Raft is a new consensus algorithm that is designed to be used instead of Paxos whilst being easier to understand and implement correctly. It models the distributed system as a replicated state machine using a replicated log. Raft features a strong leader, an explicit membership change mechanism and a (partial) correctness proof.

By default, Raft uses majority voting to find quorums. Both the initial paper and the separate correctness proof suggest that it should be possible to replace majority voting by other voting schemes.

Structured voting schemes impose a logical structure on the set of processes and use structural properties to specify quorum systems. They all have a notion of *write quorum*, with the property that any two write quorums overlap. The Grid protocol, for example, logically arranges all nodes in a rectangular grid. A write quorum in the Grid protocol consists of the union of one entire row of nodes and one entire column of nodes. Clearly, any two such write quorums intersect. Since this intersection property is the only thing the correctness proof requires, replacing majority consensus by a write quorum computed from a structured voting scheme would not affect the correctness of the Raft algorithm.

Structured voting protocols have the drawback that they may fail to generate a quorum even though more than half of the nodes are ready. They may, on the other hand, find a quorum even when less than half of the nodes are ready. Majority voting will always succeed in finding a quorum in the former case, and it will necessarily fail to do so in the latter case. Thus structured voting schemes allow for a trade-off, being less reliable than majority voting if most nodes are available, but more reliable if the majority of nodes is not available. It should be noted that structured voting protocols scale better than majority voting when the distributed system consists of a large number of nodes; when there are few nodes, majority voting tends to perform better.

The aim of this project is twofold: To add structured voting protocols to an existing implementation of the Raft consensus algorithm, and to compare the perfomance of different structured voting protocols with the performance of Raft's default majority consensus voting.

Starting Point
--------------

- Raft: I have listened to a recorded lecture on Raft by one of its creators. I have skimmed through the relevant sections of the paper and the seperate correctness prooof.
- Rafter: Several implementations of the Raft consensus algorithm exist. One of them is *Rafter*, an open source Erlang library written by Andrew J. Stone at Basho, Inc. It is a work-in-progress, but mostly feature-complete, implementation of Raft. I will use its code as a basis for this project.
- Concurrent & Distributed Systems: The IB lecture courses *Concurrent Systems* and *Distributed Systems* have given me an appreciation for the challenges that such systems pose. During my internship this summer, I implemented a minimalistic distributed key-value store that used structured voting protocols to guarantee consistency as a proof of concept in Node.js/Javascript.
- Programming: Most of the code I will be writing for this project will be in Erlang, a language I have no prior experience with. However, I am familiar with functional programming from both the IA Standard ML exercises and from small personal projects in Haskell and OCaml.

Substance and Structure of the Project
--------------------------------------

The aim of this project is to add structured voting protocols to Rafter, and to benchmark different structured voting protocols against each other and against Raft's default majority consensus algorithm. The project has the following main sections:

1. Familiarisation with the programming language Erlang, Raft, and the Rafter source code.
2. Detailed design of the data structure to represent voting structures and the algorithms to generate and interpret them. Developing and testing the code to generate and interpret voting structures separately.
3. Incorporating the above algorithms and data structures into Rafter so they can be used to find quorums.
4. Developing and testing a benchmark suite for a distributed key-value store on top of Rafter.
5. Setting up the project so that it can be run as a distributed key-value store in the Amazon EC2 infrastructure and running the benchmark test on an Amazon EC2 cluster.
6. Writing the dissertation.

Variations
----------

- Heterogeneous voting scheme configuration
- Advanced key-value store: Queries, transactions
- Profiling and performance tuning
- ?

References
----------

- Diego Ongaro and John Ousterhout, 2013: *In Search of an Understandable Consensus Algorithm* (draft)
- Anonymous, 2013: *Safety Proof and Formal Specification for Raft* (draft)
- Christian Storm, 2011: *Specification and Analytical Evaluation of Heterogeneous Dynamic Quorum-based Data Replication Schemes*

Success Criteria
----------------

To demonstrate that the project is capable of forming safe quorums using structured voting protocols without much overhead.

Timetable and Milestones
------------------------

Before Proposals Submission
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Discussion with Overseers and Director of Studies. Allocation of and discussion with Project Supervisor, preliminary reading, writing Project Proposal. Discussion with Supervisor to arrange a schedule of regular meetings for obtaining support during the course of the year.

Milestones: Phase 1 Report Form on Monday 14, 2013, then a Project Proposal complete with as realistic a timetable as possible by Wednesday 17, 2013, approval from Overseers and confirmed availability of any special resources needed. Signatures from Supervisor and Director of Studies.

Weeks 1 and 2 (Oct 21 to Nov 3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Familiarisation with Erlang: I will read the relevant parts of the introductory Erlang book *Learn You Some Erlang for Great Good*, get an overview over the Erlang and OTP documentation, and write small example programs in Erlang.

Weeks 3 to 6 (Nov 3 to Dec 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Designing the voting structure data representation and implement generators for majority consensus, the grid and the tree quorum protocol as well as an algorithm to interpret voting structures. For debugging, I will write a tool that allows a visual representation of the generated voting structures.

Weeks 7 and 8 (Dec 2 to Dec 15)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Incorporate voting structures into Rafter.

Weeks 9 to 11 (Dec 16 to Jan 5)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Christmas break.

Week 12 (Jan 6 to Jan 12)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finish incorporating voting structures into Rafter and testing.

Weeks 13 and 14 (Jan 13 to Jan 26)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write progress report and create presentation

Weeks 15 and 16 (Jan 27 to Feb 9)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Design and write benchmarking suite.

Weeks 17 and 18 (Feb 10 to Feb 23)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Setup and configuration for Amazon EC2. Benchmarking.

Weeks 19 and 20 (Feb 24 to Mar 9)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Time for extensions / buffer.

Weeks 21 to 25 (Mar 10 to Apr 13)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write Dissertation.

Weeks 24 to 26 (Apr 14 to Apr 27)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Clean-up of code and dissertation.

Resources Required
------------------

- Amazon EC2 compute time on a sufficient number of nodes (at least 100).
- My laptop: Daily pushes to GitHub, weekly backups on external hard drive
