-module(gpaxos_acceptor).

-behaviour(gen_fsm).

%% gen_fsm API
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4,
        terminate/3, code_change/4]).

%% States
-export([phase1B/2, phase2BClassic/2, phase2BFast/2]).

-type ballot() :: integer().
-type cstruct() :: list(term()).

-record(state, {
        bal  = 0 :: ballot(), % current ballot
        cbal = 0 :: ballot(), % latest ballot which this acceptor casted vote
        cval = undefined :: undefined | cstruct()  % accepted at ballot cbal
    }).

init([]) ->
    {ok, phase1B, #state{}}.

phase1B(_, St=#state{}) ->
    {next_state, phase1B, St}.

phase2BClassic(_, St=#state{}) ->
    {next_state, phase2Classic, St}.

phase2BFast(_, St=#state{}) ->
    {next_state, phase2Fast, St}.


handle_info(_, State, St) ->
    {next_state, State, St}.

handle_event(_, State, St=#state{}) ->
    {next_state, State, St}.

handle_sync_event(_, _, State, St=#state{}) ->
    {reply, {error, incorrect_call}, State, St}.

terminate(_, _, _) ->
    ok.

code_change(_OldVsn, StateName, St, _Extra) ->
    {ok, StateName, St}.
