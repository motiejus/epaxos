-module(paxos_learner).

-behaviour(gen_server).

%% API
-export([learn/2]).

%% gen_server API
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
        terminate/2, code_change/3]).

-record(state, {
        acceptors :: list(pid()),

        % {N, Value} -> no. of acceptors that gave this pair
        learns = orddict:new() :: orddict:orddict()
    }).


%% =============================================================================
%% API
%% =============================================================================

learn(ServerRef, {Acceptor, N, Val}) ->
    gen_server:cast(ServerRef, {learn, Acceptor, N, Val}).


%% =============================================================================
%% gen_server API
%% =============================================================================

init(Acceptors) ->
    {ok, #state{acceptors=Acceptors}}.

handle_cast({learn, _Acceptor, N, Val},
        #state{learns=Learns, acceptors=Acceptors}=State) ->
    NewLearns = orddict:update_counter({N, Val}, 1, Learns),
    case orddict:fetch({N, Val}, NewLearns) of
        Num when Num >= length(Acceptors) div 2 + 1 ->
            io:format("Learned ~p: ~p~n", [N, Val]),
            {stop, {learned, N, Val}};
        _ ->
            {noreply, State#state{learns=NewLearns}}
    end.

handle_info(_, State) -> {stop, bad_info, State}.
handle_call(_, _, State) -> {stop, bad_call, State}.
terminate(_, _) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
