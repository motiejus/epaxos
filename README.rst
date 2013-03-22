Classic Paxos library implementation
====================================

This is a Classic Paxos implementation in Erlang which primary goal is the
ability to easily integrate it to your application.

In order to use it, add the following dependency to ``rebar.config``::

    {deps, [
        {epaxos, "", "git://github.com/Motiejus/epaxos.git",
            {tag, "0.1"}}
        ]
    }.


Quick start
-----------

In order to start the election, you have to start the electorate first. You have
to know the number of learners, acceptors and proposers you want to have.

Start the electorate::

    Fun = fun(Value) ->
        lager:info("Learnt ~p", [Value]).

    start_electorate() ->
        Learners = epaxos:start_learners(?NUM_LEARNERS, ?NUM_ACCEPTORS, Fun),
        Acceptors = epaxos:start_acceptors(?NUM_ACCEPTORS, Learners),
        [P1|_] = Proposers = epaxos:start_proposers(?NUM_PROPOSERS, Acceptors).

``Fun`` will be called whenever a value is learnt.

Propose a value
---------------

In order to propose a value, call ``paxos_proposer:propose/2`` with a reference
to the learner and the value::

    [P1, P2|_] = Proposers,
    paxos_proposer:propose(P1, "value 1"),
    paxos_proposer:propose(P2, "value 2"),

When the value is chosen, your ``Fun`` will be called. That's it!

In progress
===========

These things are in progress:

1. Electorate dynamic reconfiguration.
2. PropEr tests with message passing failure scenarios.
