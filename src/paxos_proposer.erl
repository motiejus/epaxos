-module(paxos_proposer).

-include("include/paxos.hrl").

-behaviour(gen_fsm).

%% API
-export([propose/2, promise/3]).

%% gen_fsm API
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4,
        terminate/3, code_change/4]).

%% States
-export([phase1/2, phase2/2]).

-record(state, {
        acceptors :: list(pid()),

        % list of acceptors that promised in phase1
        promisers = [] :: list(pid()),

        % next number for this proposer is n + PAXOS_MOD
        n :: non_neg_integer(),

        % Value that might be proposed in phase2
        val :: term()
    }).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Executed by application in order to propose a value
propose(FsmRef, Value) ->
    gen_fsm:send_event(FsmRef, {propose, Value}).

%% @doc Called by acceptor on promise that it will not accept any proposals
%% numbered less than N, with the highest PropN it has accepted
promise(FsmRef, PropN, ValN) ->
    gen_fsm:send_event(FsmRef, {promise, PropN, ValN}).

%% =============================================================================
%% gen_fsm API
%% =============================================================================

init({N, Acceptors}) ->
    {ok, phase1, #state{n=N, acceptors=Acceptors}}.

phase1({propose, Value}, #state{n=N, acceptors=Acceptors}=State) ->
    NewN = N + ?PAXOS_MOD,
    lists:foreach(
        fun(Acceptor) ->
                paxos_acceptor:prepare(Acceptor, NewN, self())
        end,
        Acceptors
    ),
    {next_state, phase2, State#state{n=NewN, val=Value}}.

phase2({promise, NP, ValP}, #state{promisers=Promisers, acceptors=Acceptors,
        n=N, val=Val}) when length(Promisers) >= length(Acceptors) div 2 + 1 ->
    {NewN, NewVal} = new_val(NP, N, ValP, Val),
    lists:foreach(
        fun(Acceptor) ->
                paxos_acceptor:accept(Acceptor, NewN, NewVal)
        end,
        Acceptors
    ),
    {stop, {majority_there, NewN, NewVal}}.

handle_info(_, _, State) -> {stop, bad_info, State}.
handle_event(_, _, State) -> {stop, bad_event, State}.
handle_sync_event(_, _, _, State) -> {stop, bad_sync_event, State}.
terminate(_, _, _) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.


%% =============================================================================
%% Helpers
%% =============================================================================

new_val(NP, _N, undefined, Val) ->
    {NP, Val};
new_val(NP, N, ValP, _Val) when NP > N ->
    {NP, ValP};
new_val(_NP, N, _ValP, Val) ->
    {N, Val}.
