%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% relationship service common utilities
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(rls_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([now_usec/0,
         timestamp_to_usec/1,
         fix_case/1]).


%% now_usec/0
%% ====================================================================
%% @doc returns the current time in usecs
%% @end
-spec now_usec() -> Result :: integer().
now_usec() ->
    timestamp_to_usec(os:timestamp()).

%% timestamp_to_usec/0
%% ====================================================================
%% @doc returns the current time in usecs
%% @end
-spec timestamp_to_usec(Time :: erlang:timestamp()) -> Result :: integer().
timestamp_to_usec({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.


fix_case(Text) ->
    capfirst(string:to_lower(Text)).

%% ====================================================================
%% Internal functions
%% ====================================================================
capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.
