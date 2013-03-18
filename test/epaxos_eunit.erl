-module(epaxos_eunit).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_PROPOSERS, 3).
-define(NUM_ACCEPTORS, 3).
-define(NUM_LEARNERS, 3).

setup() ->
    error_logger:tty(false),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(lager),
    lager:set_loglevel(lager_console_backend, debug).

shutdown(_) ->
    ok = application:stop(lager),
    ok = application:stop(compiler),
    ok = application:stop(syntax_tools).

basic_test_() ->
    {setup,
        fun setup/0,
        fun shutdown/1,
        [
            fun basic_one_value/0
        ]
    }.

basic_one_value() ->
    Learners = start_learners(?NUM_LEARNERS, ?NUM_ACCEPTORS),
    Acceptors = start_acceptors(?NUM_ACCEPTORS, Learners),
    [Proposer|_] = Proposers = start_proposers(?NUM_PROPOSERS, Acceptors),
    paxos_proposer:propose(Proposer, yadda),
    timer:sleep(100),
    Proposers,
    ok.


start_learners(NumLearners, NumAcceptors) ->
    lists:map(
        fun(_) ->
                {ok, Pid} = gen_server:start_link(paxos_learner, [NumAcceptors], []),
                Pid
        end,
        lists:seq(1, NumLearners)
    ).

start_acceptors(NumAcceptors, Learners) ->
    lists:map(
        fun(_) ->
                {ok, Pid} = gen_server:start_link(paxos_acceptor, [Learners], []),
                Pid
        end,
        lists:seq(1, NumAcceptors)
    ).

start_proposers(NumProposers, Acceptors) ->
    lists:map(
        fun(N) ->
                {ok, Pid} = gen_server:start_link(paxos_proposer, [N, Acceptors], []),
                Pid
        end,
        lists:seq(1, NumProposers)
    ).
