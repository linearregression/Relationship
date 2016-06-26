%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% DB functionality for relationship service
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------


-module(rls_db).

-include("rls_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_entity/3,
         delete_entity/2,
         set_entity_property/4,
         get_entity_property/3,
         get_entity_properties/2,
         delete_entity_property/3,
         get_followed_last_posts/6,
         get_relationship_count/6,
         set_relationship_property/7,
         delete_relationship_property/6
        ]).

-export([create_staged_relationship/5,
         update_staged_relationship/7,
         get_relationship_properties/5,
         get_staged_relationships/7,
         delete_staged_relationship/4
        ]).

-export([get_indexes/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([get_follower_entities_with_offset/5,
         get_followed_entities_with_offset/5,
         get_followed_last_posts_with_offset/6
        ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% create_entity/3
%% ====================================================================
%% @doc create an entity set the properties
%% @end
-spec create_entity(UUID, Properties, DBConnection) ->
          Result when
          UUID          :: uuid(),
          DBConnection  :: term(),
          Properties    :: property_list(),
          Result :: property_list()
                    | notfound
                    | {error, Reason},
          Reason :: term().
create_entity(UUID, Properties, DBConnection) ->
    SetList = make_property_set_list("entity", "ON CREATE SET", Properties),
    Query = io_lib:format("MERGE (entity:~s { UUID:'~s' }) "
                          "~s "
                          "RETURN entity",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID,
                           SetList]),
    get_entity_properties_from_cypher_reply(
      exec_proplist_query(Query, DBConnection)).

%% delete_entity/2
%% ====================================================================
%% @doc delete an entity and all relations
%% @end
-spec delete_entity(UUID, DBConnection) ->
          Result when
          UUID          :: uuid(),
          DBConnection  :: term(),
          Result :: ok
                    | {error, term()}.
delete_entity(UUID, DBConnection) ->
     Query = io_lib:format("match (n:~s { UUID: '~s' }) "
                            "OPTIONAL MATCH (n:~s { UUID: '~s' })-[r]-() "
                            "delete n, r",
                            [?RLS_NODE_LABEL_ENTITY,
                             UUID,
                             ?RLS_NODE_LABEL_ENTITY,
                             UUID]),
    case exec_proplist_query(Query, DBConnection) of
        []   -> ok;
        Else -> Else
    end.

%% set_entity_property/4
%% ====================================================================
%% @doc Set a property for an entity
%% @end
-spec set_entity_property(UUID, Property, Value, DBConnection) ->
          Result when
          UUID          :: uuid(),
          Property      :: string(),
          Value         :: term(),
          DBConnection  :: term(),
          Result :: property_list()
                    | {error, Reason},
          Reason :: term().

set_entity_property(UUID, Property, Value, DBConnection) ->
    Query = io_lib:format("MATCH (n:~s { UUID:'~s'}) "
                          "SET n.~s = ~p "
                          "RETURN n",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID,
                           Property,
                           Value]),
    get_entity_properties_from_cypher_reply(
      exec_proplist_query(Query, DBConnection)).

delete_entity_property(UUID, Property, DBConnection) ->
    Query = io_lib:format("MATCH (n:~s { UUID:'~s'}) "
                          "REMOVE n.~s "
                          "RETURN n",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID,
                           Property]),
    get_entity_properties_from_cypher_reply(
        exec_proplist_query(Query, DBConnection)).

%% get_entity_property/3
%% ====================================================================
%% @doc Get a property for an entity
%% @end
-spec get_entity_property(UUID, Property, DBConnection) ->
          Result when
          UUID          :: uuid(),
          Property      :: string(),
          DBConnection  :: term(),
          Result :: null            % in case the proprty is missing
                    | notfound      % in case the entity is missing
                    | {error, Reason}
                    | term(),
          Reason :: term().
get_entity_property(UUID, Property, DBConnection) ->
    Query = io_lib:format("MATCH (n:~s { UUID:'~s'}) "
                          "RETURN n.~s ",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID,
                           Property]),
    case exec_proplist_query(Query, DBConnection) of
        {error, _} = R  -> R;
        [[Value]]       -> Value;
        _               -> notfound
    end.

%% get_entity_properties/2
%% ====================================================================
%% @doc Get a property for an entity
%% @end
-spec get_entity_properties(UUID, DBConnection) ->
          Result when
          UUID          :: uuid(),
          DBConnection  :: term(),
          Result :: notfound      % in case the entity is missing
                    | {error, Reason}
                    | property_list(),
          Reason :: term().
get_entity_properties(UUID, DBConnection) ->
    Query = io_lib:format("MATCH (n:~s { UUID:'~s'}) "
                          "RETURN n",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID]),
    get_entity_properties_from_cypher_reply(
      exec_proplist_query(Query, DBConnection)).

%% set_relationship_property/5
%% ====================================================================
%% @doc Set a property for an edge
%% @end
-spec set_relationship_property(Origin, Target, Category, Direction,
                                 Property, Value, DBConnection) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          Value :: term(),
          Direction :: atom(),
          DBConnection :: term(),
          Result :: notfound
                    | property_list()
                    | []
                    | {error, Reason},
          Reason :: term().
set_relationship_property(Origin, Target, Category, Direction, Property, Value,
                          DBConnection) ->
    Query = io_lib:format("match (a:~s {UUID:'~s'})-[rel:~s]~s"
                          "(b:~s {UUID:'~s'}) "
                          "set rel.~s = ~p "
                          "return rel",
                          [?RLS_NODE_LABEL_ENTITY,
                           Origin,
                           Category,
                           direction_str(Direction),
                           ?RLS_NODE_LABEL_ENTITY,
                           Target,
                           Property,
                           Value]),
    get_entity_properties_from_cypher_reply(
      exec_proplist_query(Query, DBConnection)).

%% delete_relationship_property/5
%% ====================================================================
%% @doc Delete a property for an edge
%% @end
-spec delete_relationship_property(Origin, Target, Category, Direction,
                                   Property, DBConnection) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          Direction :: atom(),
          DBConnection :: term(),
          Result :: notfound
                    | {ok, property_list()}
                    | {ok, []}
                    | {error, Reason},
          Reason :: term().
delete_relationship_property(Origin, Target, Category, Direction,
                                   Property, DBConnection) ->
    Query = io_lib:format("match (a:~s {UUID:'~s'})-[rel:~s]~s"
                          "(b:~s {UUID:'~s'}) "
                          "REMOVE rel.~s "
                          "return rel",
                          [?RLS_NODE_LABEL_ENTITY,
                           Origin,
                           Category,
                           direction_str(Direction),
                           ?RLS_NODE_LABEL_ENTITY,
                           Target,
                           Property]),
    get_entity_properties_from_cypher_reply(
        exec_proplist_query(Query, DBConnection)).

%% get_relationship_properties/5
%% ====================================================================
%% @doc Get a properties for an edge
%% @end
-spec get_relationship_properties(Origin, Target, Category, Direction,
                                  DBConnection) -> Result
    when  Origin :: string(),
          Target :: string(),
          Category :: string(),
          Direction :: atom(),
          DBConnection :: term(),
          Result :: notfound
                    | property_list()
                    | []
                    | {error, Reason},
          Reason :: term().

get_relationship_properties(Origin, Target, Category, Type, DBConnection) ->
  case Origin =:= Target of
    true ->
      {error, same_entity};
    _ ->
      Query = io_lib:format("MATCH (a:~s {UUID:'~s'})-[r:~s]~s"
      "(b:~s {UUID:'~s'}) return r",
        [?RLS_NODE_LABEL_ENTITY,
          Origin,
          Category,
          direction_str(Type),
          ?RLS_NODE_LABEL_ENTITY,
          Target]),
      get_entity_properties_from_cypher_reply(
        exec_proplist_query(Query, DBConnection))
  end.

%% get_followed_last_posts/6
%% ====================================================================
%% @doc Returns the ordered list of post IDs of followed entities
%% @end
-spec get_followed_last_posts(UUID,
                              FromTime,
                              LastPostedTimestamp,
                              Limit,
                              Type,
                              DBConnection) ->
          Result when
          UUID                :: uuid(),
          FromTime            :: now | rls_timestamp(),
          LastPostedTimestamp :: rls_timestamp(),
          Limit               :: integer(),
          Type                :: string(),
          DBConnection        :: term(),
          Result :: []
                    | [{FUUID       :: uuid(),
                        PostId      :: post_id(),
                        PostId      :: post_id(),
                        TimeStamp   :: rls_timestamp()}]
                    | {error, Reason},
          Reason :: term().
get_followed_last_posts(UUID,
                        FromTime,
                        LastPostedTimestamp,
                        Limit,
                        Type,
                        DBConnection) ->
    Query = case FromTime of
                now ->
                    io_lib:format("match (node:~s {UUID:\"~s\"})-[rel:~s]->"
                                  "(other:~s) "
                                  "where other.last_posted_timestamp >= ~p "
                                  "and rel.Stage = 'active' "
                                  "with other "
                                  "order by other.last_posted_timestamp desc "
                                  "limit ~p "
                                  "return other.UUID, "
                                  "other.last_post_id, "
                                  "other.last_posted_timestamp",
                                  [?RLS_NODE_LABEL_ENTITY,
                                   list_to_binary(UUID),
                                   Type,
                                   ?RLS_NODE_LABEL_ENTITY,
                                   LastPostedTimestamp,
                                   Limit]);
                Time ->
                    io_lib:format("match (node:~s {UUID:\"~s\"})-[rel:~s]->"
                                  "(other:~s) "
                                  "where other.last_posted_timestamp <= ~p and "
                                  "other.last_posted_timestamp >= ~p and"
                                  "and rel.Stage = 'active' "
                                  "with other "
                                  "order by other.last_posted_timestamp desc "
                                  "limit ~p "
                                  "return other.UUID, "
                                  "other.last_post_id, "
                                  "other.last_posted_timestamp",
                                  [?RLS_NODE_LABEL_ENTITY,
                                   list_to_binary(UUID),
                                   Type,
                                   ?RLS_NODE_LABEL_ENTITY,
                                   Time,
                                   LastPostedTimestamp,
                                   Limit])
            end,
    %% remove the entries that may not have posted yet so {nill, 0}
    case exec_proplist_query(Query, DBConnection) of
        {error, Reason} ->
            {error, Reason};
        Proplist ->
            lists:filter(fun({_, null, _}) -> false;
                            (_)            -> true
                         end,
                         make_proplist_from_list_list(Proplist))
    end.

%% create_staged_relationship/5
%% ====================================================================
%% @doc Returns the entities followed, ordered by Full Name
%% @end
-spec create_staged_relationship(Origin, Target, Type,
                                 Proplist, DBConnection) ->
          Result when
          Origin        :: string(),
          Target        :: string(),
          Type          :: string(),
          Proplist      :: property_list(),
          DBConnection  :: term(),
          Result :: ok
                  | {error, Reason},
          Reason :: term().
create_staged_relationship(Origin, Target, Category, Proplist, DBConnection) ->
    case Origin =:= Target of
        true ->
            {error, same_entity};
        _ ->
            SetList = make_property_set_list("rel", "ON CREATE SET", Proplist),
            Query = io_lib:format("MATCH (a:~s {UUID:'~s'}),(b:~s {UUID:'~s'}) "
                                  "MERGE (a)-[rel:~s]->(b) "
                                  "~s "
                                  "return rel.Stage",
                                  [?RLS_NODE_LABEL_ENTITY,
                                   Origin,
                                   ?RLS_NODE_LABEL_ENTITY,
                                   Target,
                                   Category,
                                   SetList]),
            case exec_proplist_query(Query, DBConnection) of
                {error, _} = R ->
                    R;
                [[Value]] ->
                    list_to_atom(binary_to_list(Value));
                _ -> {error, notfound}
            end
    end.

update_staged_relationship(Origin, Target, Category, Type, CurrentStage,
                           Proplist, DBConnection) ->
%%     ?LOGFORMAT(info,"Update staged. Origin:~p, Target:~p, Current:~p, Props:~p",
%%                [Origin, Target, CurrentStage, Proplist]),
    case Origin =:= Target of
        true ->
            {error, same_entity};
        _ ->
            case CurrentStage of
                empty ->
                    create_staged_relationship(Origin,
                                               Target,
                                               Category,
                                               Proplist,
                                               DBConnection);
                _Else ->
                    SetList = make_property_set_list("rel", "SET", Proplist),
                    Query = io_lib:format("match (a:~s {UUID:'~s'})-[rel:~s]~s"
                                          "(b:~s {UUID:'~s'}) "
                                          "where rel.Stage = '~p' "
                                          "~s "
                                          "return rel.Stage",
                                          [?RLS_NODE_LABEL_ENTITY,
                                           Origin,
                                           Category,
                                           direction_str(Type),
                                           ?RLS_NODE_LABEL_ENTITY,
                                           Target,
                                           CurrentStage,
                                           SetList]),
                    case exec_proplist_query(Query, DBConnection) of
                        {error, _} = R ->
                            R;
                        [[Value]] ->
                            list_to_atom(binary_to_list(Value));
                        _ -> {error, notfound}
                    end
            end
    end.

get_staged_relationships(Stage,
                         UUID,
                         Category,
                         {StartName, StartUUID},
                         PageSize,
                         Direction,
                         DBConnection) when is_binary(StartName) and
                                            is_binary(StartUUID) ->
    get_staged_relationships(Stage, UUID, Category,
                             {binary_to_list(StartName),
                              binary_to_list(StartUUID)},
                             PageSize, Direction, DBConnection);

get_staged_relationships(Stage,
                         UUID,
                         Category,
                         {StartName, StartUUID},
                         PageSize,
                         Direction,
                         DBConnection) ->
    OriginDir = case Direction of
                    origin    -> "<-";
                    target    -> "-";
                    undefined -> "-"
                end,
    TargetDir = case Direction of
                    origin    -> "-";
                    target    -> "->";
                    undefined -> "-"
                end,
    Query = io_lib:format("match (node:~s {UUID:'~s'})~s[rel:~s]~s"
                          "(other:~s) "
                          "where other.full_name_UUID > '~s' "
                          "and rel.Stage = '~s' "
                          "with other "
                          "order by other.full_name_UUID "
                          "limit ~p return other.entity_full_name, "
                          "other.UUID",
                          [?RLS_NODE_LABEL_ENTITY,
                           UUID,
                           OriginDir,
                           Category,
                           TargetDir,
                           ?RLS_NODE_LABEL_ENTITY,
                           ?RLS_MAKE_FULLNAME_UUID(StartName, StartUUID),
                           Stage,
                           PageSize]),
    make_proplist_from_list_list(
        exec_proplist_query(Query, DBConnection)).

delete_staged_relationship(Origin, Target, Category, DBConnection) ->
    Query = io_lib:format("match (n:~s)-[rel:~s]->(r:~s) "
                          "where n.UUID=\"~s\" and r.UUID=\"~s\" "
                          "delete rel",
                          [?RLS_NODE_LABEL_ENTITY,
                           Category,
                           ?RLS_NODE_LABEL_ENTITY,
                           list_to_binary(Origin),
                           list_to_binary(Target)]),
    case exec_proplist_query(Query, DBConnection) of
        {error, _} = R -> R;
        []             -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get_follower_entities_with_offset/5
%% ====================================================================
%% @doc Returns the follower entities, ordered by Full Name starting
%% from an offset
%% @end
-spec get_follower_entities_with_offset(UUID,
                                        Offset,
                                        Limit,
                                        Type,
                                        DBConnection) ->
          Result when
          UUID          :: uuid(),
          Offset        :: offset(),
          Limit         :: integer(),
          Type          :: string(),
          DBConnection  :: term(),
          Result :: []
                    | [{Name    :: binary(),
                        FUUID   :: uuid()}]
                    | {error, Reason},
          Reason :: term().
get_follower_entities_with_offset(UUID,
                                  Offset,
                                  Limit,
                                  Type,
                                  DBConnection) ->
    Query = io_lib:format("match (node:~s {UUID:\"~s\"})<-[rel:~s]-"
                          "(other:~s) "
                          "return other.entity_full_name, "
                          "other.UUID "
                          "order by other.entity_full_name "
                          "skip ~p limit ~p",
                          [?RLS_NODE_LABEL_ENTITY,
                           list_to_binary(UUID),
                           Type,
                           ?RLS_NODE_LABEL_ENTITY,
                           Offset,
                           Limit]),
    make_proplist_from_list_list(
      exec_proplist_query(Query, DBConnection)).

%% get_followed_entities_with_offset/5
%% ====================================================================
%% @doc Returns the ordered list of post IDs of followed entities starting
%% from an offset
%% @end
-spec get_followed_entities_with_offset(UUID,
                                        Offset,
                                        Limit,
                                        Type,
                                        DBConnection) ->
          Result when
          UUID          :: uuid(),
          Offset        :: offset(),
          Limit         :: integer(),
          Type          :: string(),
          DBConnection  :: term(),
          Result :: []
                    | [{Name    :: binary(),
                        FUUID   :: uuid()}]
                    | {error, Reason},
          Reason :: term().
get_followed_entities_with_offset(UUID,
                                  Offset,
                                  Limit,
                                  Type,
                                  DBConnection) ->
    Query = io_lib:format("match (node:~s {UUID:\"~s\"})-[rel:~s]->"
                          "(other:~s) "
                          "return other.entity_full_name, "
                          "other.UUID "
                          "order by other.entity_full_name "
                          "skip ~p limit ~p",
                          [?RLS_NODE_LABEL_ENTITY,
                           list_to_binary(UUID),
                           Type,
                           ?RLS_NODE_LABEL_ENTITY,
                           Offset,
                           Limit]),
    make_proplist_from_list_list(
      exec_proplist_query(Query, DBConnection)).

%% get_followed_last_posts_with_offset/6
%% ====================================================================
%% @doc Returns the ordered list of post IDs of followed entities
%% @end
-spec get_followed_last_posts_with_offset(UUID,
                                          Offset,
                                          LastPostedTimestamp,
                                          PageSize,
                                          Type,
                                          DBConnection) ->
          Result when
          UUID                :: uuid(),
          Offset              :: offset(),
          LastPostedTimestamp :: rls_timestamp(),
          PageSize            :: page_size(),
          Type                :: string(),
          DBConnection        :: term(),
          Result :: []
                    | [{FUUID       :: uuid(),
                        PostId      :: post_id(),
                        TimeStamp   :: rls_timestamp()}]
                    | {error, Reason},
          Reason :: term().
get_followed_last_posts_with_offset(UUID,
                        Offset,
                        LastPostedTimestamp,
                        PageSize,
                        Type,
                        DBConnection) ->
    Query = io_lib:format("match (node:~s {UUID:\"~s\"})-[rel:~s]->"
                          "(other:~s) "
                          "where other.last_posted_timestamp >= ~p "
                          "with other "
                          "order by other.last_posted_timestamp desc "
                          "skip ~p limit ~p "
                          "return other.UUID, "
                          "other.last_post_id, "
                          "other.last_posted_timestamp",
                          [?RLS_NODE_LABEL_ENTITY,
                           list_to_binary(UUID),
                           Type,
                           ?RLS_NODE_LABEL_ENTITY,
                           LastPostedTimestamp,
                           Offset,
                           PageSize]),
    %% remove the entries that may not have posted yet so {nill, 0}
    case exec_proplist_query(Query, DBConnection) of
        {error, Reason} ->
            {error, Reason};
        Proplist ->
            lists:filter(fun({_, null, _}) -> false;
                            (_)            -> true
                         end,
                         make_proplist_from_list_list(Proplist))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_relationship_count(UUID, Category, Type, Stage, Direction, DBConnection) ->
    {FromStr, ToStr} = set_direction_strs(Type, Direction),
    Query = io_lib:format("match (node:~s {UUID:\"~s\"})~s[rel:~s]~s(other:~s) "
                          "where rel.Stage = '~s' "
                          "return count(other)",
                          [?RLS_NODE_LABEL_ENTITY,
                           list_to_binary(UUID),
                           FromStr,
                           Category,
                           ToStr,
                           ?RLS_NODE_LABEL_ENTITY,
                           Stage]),
    case exec_proplist_query(Query, DBConnection) of
        [[Nr]] -> Nr;
        Else   -> Else
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

make_property_set_list(NodeRef, Prelude, Properties) ->
        case length(Properties) > 0 of
            true ->
                RetVal =
                    lists:foldl(fun ({Name, Value}, Acc) ->
                                         io_lib:format(" ~s ~s.~s = ~p,",
                                                       [Acc, NodeRef,
                                                        Name, Value])
                                end,
                                Prelude, Properties),
                lists:flatten(lists:droplast(RetVal));
            _ ->
                ""
        end.

%% exec_proplist_query/2
%% ====================================================================
%% @doc execute a neo4j query and returns the property lists as tuples
%% @end
-spec exec_proplist_query(Query :: string(),
                 DBConnection :: term()) -> Result when
                 Result :: property_list()
                           | {error, Reason}
                           | list()
                           | [],
                 Reason :: term().
exec_proplist_query(Query, DBConnection) ->
    Q = list_to_binary(lists:flatten(Query)),
    case call_neo4j_cypher(DBConnection, Q) of
        Error when not is_list(Error) ->
            Error;
        CypherReply ->
            Reply =
            proplists:get_value(<<"data">>, CypherReply),
            Reply
    end.

-spec make_proplist_from_list_list(ListList :: list(list())
                                               | term()) ->
          Result when
          Result :: list(tuple())
                 | term().
make_proplist_from_list_list(ListList) when is_list(ListList) ->
    [ list_to_tuple(A) || A <- ListList];
make_proplist_from_list_list(Term) -> Term.

-spec get_entity_properties_from_cypher_reply(Cypher) ->
          Result when
          Cypher :: {error, EReason}
                    | []
                    | property_list(),
          Result :: property_list()
                    | {error, Reason},
          Reason :: term(),
          EReason :: term().
get_entity_properties_from_cypher_reply({error, Reason}) -> {error, Reason};
get_entity_properties_from_cypher_reply([]) -> {error, notfound};
get_entity_properties_from_cypher_reply(Reply) ->
    %% there should be only one node in the Reply
    %% if there are more than one, delete all but one and return that
    {Entity} =
        case Reply of
            [[Node]] ->
                Node;
            [Node|Rest] ->
                lists:foreach(fun([X]) ->
                                      neo4j:delete_node(X)
                              end,
                              Rest),
                Node
        end,
    {Proplist} =
        proplists:get_value(<<"data">>, Entity),
    Proplist.

-spec call_neo4j_cypher(DBConnection, Query) ->
          Result when
          DBConnection :: term(),
          Query :: term(),
          Result :: property_list()
                    | {error, Reason},
          Reason :: term().
call_neo4j_cypher(DBConnection, Query) ->
    try neo4j:cypher(DBConnection, Query) of
      {PropList} ->
            PropList
    catch
        _:_ ->
            {error, {neo4j, erlang:get_stacktrace()}}
    end.

set_direction_strs(Type, Direction) ->
    case Type of
        one_way ->
            case Direction of
                targets   -> {"-", "->"};
                origins   -> {"<-", "-"};
                undefined -> {"-", "-"}
            end;
        two_way ->
            {"-", "-"}
    end.

direction_str(Direction) ->
    case Direction of
        one_way -> "->";
        two_way -> "-"
    end.

get_indexes(DBConnection) ->
    Query = io_lib:format(":schema",[]),
    exec_proplist_query(Query, DBConnection).

