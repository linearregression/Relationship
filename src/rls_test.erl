%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% basic test functions done by hand
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(rls_test).

-include("rls_common.hrl").
-include("rls.hrl").

-define(UUID_PREFIX, "SQOR").
-define(MAKE_UUID(Number), ?UUID_PREFIX ++ integer_to_list(Number)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_entity/1,
         delete_entity/1,
         create_nodes/1,
         create_nodes_with_stat/2,
         get_entity_data/1,
         update_random_timestamp/2,
         get_last_posts/1,
         get_last_posts/2,
         nr_of_followers/1,
         nr_of_followed/1,
         unfollow/2,
         follow/2,
         get_followers/1,
         get_followers/2,
         get_followers/3,
         get_followed/1,
         get_followed/2,
         get_followed/3,
         get_followed_with_offset/2,
         get_followers_with_offset/2,
         test_get_last_posted/1,
         test_get_last_posted/2
        ]).

get_entity_data(UUID) ->
    rls:get_entity_properties(?MAKE_UUID(UUID)).

create_entity(UUID) ->
    rls:create_entity(?MAKE_UUID(UUID),
                      ?UUID_PREFIX ++ " " ++ integer_to_list(UUID)
                     ).

delete_entity(UUID) ->
    rls:delete_entity(?MAKE_UUID(UUID)).

create_relationship(UUID, FollowerUUID, _Type) ->
    rls:follow_entity(?MAKE_UUID(UUID),
                      ?MAKE_UUID(FollowerUUID)).

create_nodes(0) -> ok;
create_nodes(NrOfNodes) ->
    _ = statistics(runtime),
    _ = statistics(wall_clock),
    create_nodes_with_stat(NrOfNodes, NrOfNodes),
    {_, T1} = statistics(runtime),
    {_, T2} = statistics(wall_clock),
    io:format("Created ~p nodes. Run time was ~p (~p)", [NrOfNodes,
                                                         T1, T2]).

create_nodes_with_stat(0, _) ->
    ok;

create_nodes_with_stat(NrOfNodes, MaxNrOfNodes) ->
    _ = create_entity(NrOfNodes),
    create_relationships(NrOfNodes, MaxNrOfNodes, ?RLS_RELATIONSHIP_FOLLOW),
    case (NrOfNodes rem 100) =:= 0 of
        true ->
            io:format(".");
        _ ->
            ok
    end,
    create_nodes_with_stat(NrOfNodes - 1, MaxNrOfNodes).

create_relationships(A, A, _) -> ok;
create_relationships(NodeNr, MaxNrOfNodes, Type) ->
    _ = create_relationship(NodeNr, MaxNrOfNodes, Type),
    create_relationships(NodeNr, MaxNrOfNodes - 1, Type).

update_random_timestamp(_, 0) -> ok;
update_random_timestamp(NrOfNodes, NrOfUpdated) ->
    A = random:uniform(NrOfNodes),
    _ = rls:update_last_post_timestamp(?MAKE_UUID(A),
                                       os:timestamp(),
                                       integer_to_list(A)),
    update_random_timestamp(NrOfNodes, NrOfUpdated - 1).


get_last_posts(UUID) ->
    rls:get_followed_last_posts(?MAKE_UUID(UUID)).

get_last_posts(UUID, Limit) ->
    rls:get_followed_last_posts(?MAKE_UUID(UUID),
                                now,
                                0,
                                Limit).

get_followers(UUID) ->
    rls:get_follower_entities(?MAKE_UUID(UUID)).

get_followers(UUID, StartName) ->
    rls:get_follower_entities(?MAKE_UUID(UUID),
                              StartName).

get_followers(UUID, StartName, Limit) ->
    rls:get_follower_entities(?MAKE_UUID(UUID),
                              StartName,
                              Limit).
get_followers_with_offset(UUID, Offset) ->
    rls:get_follower_entities_with_offset(?MAKE_UUID(UUID),
                                          Offset).

get_followed(UUID) ->
    rls:get_followed_entities(?MAKE_UUID(UUID)).

get_followed(UUID, StartName) ->
    rls:get_followed_entities(?MAKE_UUID(UUID),
                                          StartName).

get_followed(UUID, StartName, Limit) ->
    rls:get_followed_entities(?MAKE_UUID(UUID),
                              StartName,
                              Limit).

get_followed_with_offset(UUID, Offset) ->
    rls:get_followed_entities_with_offset(?MAKE_UUID(UUID),
                                          Offset).

follow(UUID1, Follower) ->
    rls:follow_entity(?MAKE_UUID(UUID1),
                      ?MAKE_UUID(Follower)).

unfollow(UUID1, UUID2) ->
    rls:unfollow_entity(?MAKE_UUID(UUID1),
                        ?MAKE_UUID(UUID2)).

nr_of_followed(UUID) ->
    rls:nr_of_followed_entities(?MAKE_UUID(UUID)).

nr_of_followers(UUID) ->
    rls:nr_of_follower_entities(?MAKE_UUID(UUID)).

test_get_last_posted(Max) ->
    test_get_last_posted(Max, Max).

test_get_last_posted(0, _) ->
    io:format("~n",[]),
    ok;
test_get_last_posted(Count, Max) ->
    A = random:uniform(Max),
    _ = rls:get_followed_last_posts_p(?MAKE_UUID(A), now, 0, 25),
    case (Count rem 100) =:= 0 of
        true ->
            io:format("\e[s\e[K~p\e[u", [Count]);
        _ ->
            ok
    end,
    test_get_last_posted(Count - 1, Max).

%% ====================================================================
%% Internal functions
%% ====================================================================