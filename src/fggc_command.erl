-module(fggc_command).

-type source() :: non_neg_integer().
-type seq() :: non_neg_integer().
-type key() :: non_neg_integer().

-record(command, {
        source :: source(), % Command
        seq :: seq(), % Command
        key :: key(), % CommutativeCommand
        checkpointNumber :: fggc_types:checkpointNumber() % CheckpointCommand
    }).

-opaque command() :: #command{}.
-export_type([command/0]).

-export([new/1, source/1, sequenceNumber/1, compare/2]).

%% CheckpointCommand:23 makes source and key = 0, and seems like
%% all constructors are using CheckPointCommand (no Commands are
%% generated explicitly, only CheckpointCommands).
-spec new(source()) -> command().
new(Src) ->
    #command{source=Src, seq=next_unique_counter_for(Src)}.

-spec source(command()) -> source().
source(#command{source=Src}) ->
    Src.

-spec sequenceNumber(command()) -> seq().
sequenceNumber(#command{seq=Seq}) ->
    Seq.

-spec compare(command(), command()) -> -1 | 0 | 1.
compare(#command{source=Src1, seq=Seq1},
        #command{source=Src2, seq=Seq2}) ->
    cmp({Src1, Seq1}, {Src2, Seq2}).

%% =============================================================================
%% Helpers
%% =============================================================================

next_unique_counter_for(source()) -> non_neg_integer().
next_unique_counter_for(Src) ->
    ets:insert_new(fggc_counters, {Src, -1}),
    ets:update_counter(fggc_counters, Src, 1).

cmp(A, B) when A > B -> 1;
cmp(A, B) when A < B -> -1;
cmp(A, B) when A == B -> 0;
