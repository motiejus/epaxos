-module(fggc_cstruct).

-export([new/0]).

-record(fggc_cstruct, {
        checkpointNumber :: checkpointNumber(),
        commands :: list(fggc_command:command())
    }).
-type cstruct() :: #fggc_cstruct{}.

new() ->
    #fggc_cstruct{checkpointNumber = 0, commands = []}.

%% @doc append a command to cstruct. Inefficiently check if command
%% is not in the list already.
-spec append(cstruct(), fggc_command:command()) -> cstruct().
append(#fggc_cstruct{commands=Cmds}=CStruct, Cmd) ->
    case lists:member(Cmd, Cmds) of
        true ->
            CStruct;
        false ->
            CStruct#fggc_cstruct{commands=[Cmd|Cmds]}
    end.

-spec contains(cstruct(), command()) -> boolean().
contains(#fggc_Cstruct{commands=Cmds}, Cmd) ->
    lists:member(Cmd, Cmds).

-spec remove(cstruct(), command()) -> cstruct().
delete(#fggc_cstruct{commands=Cmds}=CStruct, Cmd) ->
    CStruct{commands=lists:delete(Cmd, Cmds)}.

%% @doc In Java this adds all elements in Rest to First.
-spec lub(list(cstruct())) -> undefined | cstruct().
lub([]) ->
    undefined;
lub([Commands]) ->
    Commands;
lub(CStructs) ->
    % What now?
    %lists:foldl(fun append/2, To, CStructs).
    ok.

-spec glb(list(cstruct())) -> undefined | cstruct().
glb([]) ->
    undefined;
glb([Commands]) ->
    Commands;
glb(CStructs) ->
    % What now?
    ok.
