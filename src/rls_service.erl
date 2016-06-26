%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% rls service callback module.
%%% behaviour and service functions
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(rls_service).
%%-behaviour(csi_server).
-include("rls_common.hrl").
-include("rls.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

-record(rls_state, {db_connection,
                    admins = [] :: list(),
                    logic = #{} :: map()}).
-record(rls_service_state, {db_connection_parameters,
                            admins = [] :: list(),
                            logic = #{} :: map()}).

-export([create_entity/2,
         delete_entity/2,
         get_entity_properties/2,
         update_last_post_timestamp/2,
         get_followed_last_posts/2,
         get_entity_property/2,
         set_entity_property/2,
         delete_entity_property/2,
         set_relationship_property/2,
         get_relationship_property/2,
         get_relationship_properties/2,
         get_relationship_specific_properties/2,
         delete_relationship_property/2
        ]).

-export([logic_clear/2,
         logic_add/2,
         logic_get/2,
         handle_call/3,
         relationship_process/2,
         relationship_stage/2,
         relationship_commands/2,
         relationship_check_stage/2,
         get_relationship_origins_for_stage/2,
         get_relationship_targets_for_stage/2,
         get_relationship_origins_count/2,
         get_relationship_targets_count/2,
         get_relationship_count/2,
         get_relationships_for_stage/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([get_followed_entities_with_offset/2,
         get_follower_entities_with_offset/2,
         get_followed_last_posts_with_offset/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_service(_InitArgs) ->
    ConnectionValue =
        case application:get_env(?RLS_APPLICATION_NAME, neo4j_connection) of
            {ok, NeoConnection} ->
                NeoConnection;
            undefined ->
                ?LOGFORMAT(error, "Environment for neo4j is not set", []),
%                [{base_uri, <<"http://127.0.0.1:7474/db/data/">>}]
                [{base_uri, <<"http://neo4j-dev.sqor.com:7474/db/data/">>}]
        end,
    ?LOGFORMAT(info,"Using:~p for neo4j server",[ConnectionValue]),
    Logic =
        case application:get_env(?RLS_APPLICATION_NAME, logic) of
            {ok, L} ->
                L;
            undefined ->
                ?LOGFORMAT(warning, "Relatinship logic is not set", []),
                #{}
        end,
    Admins =
        case application:get_env(?RLS_APPLICATION_NAME, admins) of
            {ok, L1} ->
                L1;
            undefined ->
                ?LOGFORMAT(warning, "Relatinship admins are not set", []),
                []
        end,
    {ok,
     #rls_service_state{db_connection_parameters = ConnectionValue,
                        admins = Admins,
                        logic = Logic
                       }}.

init(_Args, ServerState) ->
    {DBConnection} = neo4j:connect(
                     ServerState#rls_service_state.db_connection_parameters),
    {ok, #rls_state{db_connection = DBConnection,
                    logic = ServerState#rls_service_state.logic,
                    admins = ServerState#rls_service_state.admins}}.

terminate(Reason, _State) ->
    case Reason of
        normal ->
            ok;
        WAFIT ->
            ?LOGFORMAT(info,
                       "Relationship process terminated for reason ~p", [WAFIT])
    end.

terminate_service(_Reason, _State) ->
    ok.

%% create_entity/2
%% ====================================================================
%% @doc
%% @end
-spec create_entity(Params, State) -> {Result, NewStatus} when
          Params :: {UUID :: string(), FullName :: string()},
          State :: #rls_state{},
    Result :: property_list()
              | {error, Reason},
    Reason :: term(),
    NewStatus :: #rls_state{}.
create_entity({UUID, FullName}, State) ->
    {rls_db:create_entity(UUID,
                          [{?RLS_FULLNAME_PROPERTY_NAME, FullName},
                           {?RLS_FULLNAME_UUID_PROPERTY_NAME,
                            ?RLS_MAKE_FULLNAME_UUID(FullName, UUID)},
                           {?RLS_TIMESTAMP_PROPERTY_NAME, 0}
                          ],
                          State#rls_state.db_connection),
     State}.

%% delete_entity/2
%% ====================================================================
%% @doc
%% @end
-spec delete_entity(UUID :: string(),
                    State :: #rls_state{}) ->
          {Result, NewStatus} when
    Result :: ok
            | {error, Reason},
    NewStatus :: #rls_state{},
    Reason :: term().

delete_entity(UUID, State) ->
    {rls_db:delete_entity(UUID, State#rls_state.db_connection),
     State}.

%% get_entity_properties/2
%% ====================================================================
%% @doc
%% @end
-spec get_entity_properties(UUID :: string(),
                            State :: #rls_state{}) ->
          {Result, NewStatus} when
          Result :: EntityData
              | {error, Reason},
          EntityData :: property_list(),
          NewStatus :: #rls_state{},
          Reason :: term().

get_entity_properties(UUID, State) ->
     {rls_db:get_entity_properties(UUID,
                                   State#rls_state.db_connection),
      State}.

%% set_entity_property/2
%% ====================================================================
%% @doc Set a property for an entiy
%% @end
-spec set_entity_property({UUID, Property, Value}, State) ->
          {Result, NewStatus} when
          UUID :: string(),
          Property :: string(),
          Value :: term(),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
set_entity_property({UUID, Property, Value}, State) ->
    {rls_db:set_entity_property(UUID,
                                Property,
                                Value,
                                State#rls_state.db_connection),
     State}.

%% get_entity_property/2
%% ====================================================================
%% @doc Get a property for an entiy
%% @end
-spec get_entity_property({UUID, Property}, State) ->
          {Result, NewStatus} when
          UUID :: string(),
          Property :: string(),
          State :: #rls_state{},
          Result :: null
                    | notfound
                    | term(),
          NewStatus :: #rls_state{}.
get_entity_property({UUID, Property}, State) ->
    {rls_db:get_entity_property(UUID,
                                Property,
                                State#rls_state.db_connection),
     State}.

%% delete_entity_property/2
%% ====================================================================
%% @doc Delete a property for an entiy
%% @end
-spec delete_entity_property({UUID, Property}, State) ->
          {Result, NewStatus} when
          UUID :: string(),
          Property :: string(),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
delete_entity_property({UUID, Property}, State) ->
    {rls_db:delete_entity_property(UUID,
                                   Property,
                                   State#rls_state.db_connection),
     State}.

%==================

%% set_relationship_property/5
%% ====================================================================
%% @doc Set a property for an edge
%% @end
-spec set_relationship_property({Origin, Target, Category, Property, Value},
                                State) ->
          {Result, NewStatus} when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          Value :: term(),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
set_relationship_property({Origin, Target, Category, Property, Value}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:set_relationship_property(Origin,
                                      Target,
                                      Category,
                                      Type,
                                      Property,
                                      Value,
                                      State#rls_state.db_connection),
     State}.

%% get_relationship_property/4
%% ====================================================================
%% @doc Get a property for an edge
%% @end
-spec get_relationship_property({Origin, Target, Category, Property}, State) ->
          {Result, NewStatus} when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          State :: #rls_state{},
          Result :: undefined
                    | term()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
get_relationship_property({Origin, Target, Category, Property}, State) ->
    case get_relationship_properties({Origin,
                                      Target,
                                      Category}, State) of
        {List, NewState} when is_list(List) ->
            {proplists:get_value(list_to_binary(Property), List, undefined),
             NewState};
        Else ->
            Else
    end.
%% get_relationship_properties/2
%% ====================================================================
%% @doc Get all properties for an edge
%% @end
-spec get_relationship_properties({Origin, Target, Category}, State) ->
          {Result, NewStatus} when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
get_relationship_properties({Origin, Target, Category}, State) ->
    %% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
    %% when >= R18
    %    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:get_relationship_properties(Origin,
                                        Target,
                                        Category,
                                        Type,
                                        State#rls_state.db_connection),
     State}.

%% get_relationship_properties/2
%% ====================================================================
%% @doc Get properties for an edge
%% @end
-spec get_relationship_specific_properties({Origin, Target,
                                            Category, Properties},
                                           State) ->
          {Result, NewStatus} when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Properties :: list(string()),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
get_relationship_specific_properties({Origin, Target, Category, Properties},
                                     State) ->
    %% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
    %% when >= R18
    %    #{Category := #{ type := Type}} = State#rls_state.logic,
    case rls_db:get_relationship_properties(Origin,
                                            Target,
                                            Category,
                                            Type,
                                            State#rls_state.db_connection) of
        Props when is_list(Props) ->
            {lists:foldl(fun (X, Acc) ->
                                  case lists:keyfind(list_to_binary(X),
                                                     1, Props) of
                                      false ->
                                          [{X, undefined} | Acc];
                                      Else ->
                                          [Else | Acc]
                                  end
                         end,
                         [],
                         Properties),
             State};
        Else ->
            {Else, State}
    end.

%% delete_relationship_property/4
%% ====================================================================
%% @doc Delete a property for an edge
%% @end
-spec delete_relationship_property({Origin, Target, Category, Property}, State) ->
          {Result, NewStatus} when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          State :: #rls_state{},
          Result :: notfound
                    | property_list()
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
delete_relationship_property({Origin, Target, Category, Property}, State) ->
    %% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
    %% when >= R18
    %    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:delete_relationship_property(Origin,
                                         Target,
                                         Category,
                                         Type,
                                         Property,
                                         State#rls_state.db_connection),
     State}.

%============================

%% update_last_post_timestamp/2
%% ====================================================================
%% @doc
%% @end
-spec update_last_post_timestamp(Params, State) -> {Result, NewStatus} when
          Params :: {UUID :: string(),
                     Timestamp :: erlang:timestamp(),
                     PostId :: string()},
          State :: #rls_state{},
          Result :: property_list()
                    | notfound
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.
update_last_post_timestamp({UUID, TimeStamp, PostId}, State) ->
    Reply =
        case rls_db:set_entity_property(UUID,
                                        ?RLS_TIMESTAMP_PROPERTY_NAME,
                                        rls_utils:timestamp_to_usec(TimeStamp),
                                        State#rls_state.db_connection) of
            {error, notfound} ->
                notfound;
            {error, Reason} ->
                {error, Reason};
            _ ->
                rls_db:set_entity_property(UUID,
                                           ?RLS_POSTID_PROPERTY_NAME,
                                           PostId,
                                           State#rls_state.db_connection)
        end,
    {Reply, State}.

%% get_followed_last_posts/2
%% ====================================================================
%% @doc
%% @end
-spec get_followed_last_posts({UUID :: string(),
                               FromTime :: now | integer(),
                               LastPostedTimestamp :: integer(),
                               Limit :: integer()},
                              State  :: term()) ->
          {Result, NewStatus} when
          Result :: []
                    | [{PostId :: binary(),
                        TimeStamp :: integer()}]
                    | {error, Reason},
          Reason :: term(),
          NewStatus :: #rls_state{}.

%% ====================================================================
get_followed_last_posts({UUID, FromTime, LastPostedTimestamp, Limit}, State) ->
    {rls_db:get_followed_last_posts(UUID,
                                    FromTime,
                                    LastPostedTimestamp,
                                    Limit,
                                    ?RLS_RELATIONSHIP_FOLLOW,
                                    State#rls_state.db_connection),
                                    State}.

get_relationship_origins_count({UUID, Category}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:get_relationship_count(UUID, Category, Type, active,
                                   origins, State#rls_state.db_connection),
     State}.

get_relationship_targets_count({UUID, Category}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:get_relationship_count(UUID, Category, Type, active,
                                   targets, State#rls_state.db_connection),
     State}.

get_relationship_count({UUID, Category, Stage, Direction}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    {rls_db:get_relationship_count(UUID, Category, Type, Stage,
                                   Direction, State#rls_state.db_connection),
     State}.

logic_clear(_, State) ->
    {#{}, State#rls_service_state{logic = #{}}}.

logic_add(Logic, State) ->
    NewLogic = maps:merge(State#rls_service_state.logic, Logic),
    NewState = State#rls_service_state{logic = NewLogic},
    {NewLogic, NewState}.

logic_get([], State) ->
    {State#rls_service_state.logic, State};

logic_get(Key, State) ->
    {maps:get(Key, State#rls_service_state.logic, undefined), State}.

%% relationship_process/2
%% ====================================================================
%% @doc make a state change based on the command
%% @end
-spec relationship_process({Command, Requester, Origin, Target, Type}, State) ->
          Result when
          Command :: atom(),
          Requester :: string(),
          Origin :: string(),
          Target :: string(),
          Type :: term(),
          State :: #rls_state{},
          Result :: {ok, Stage}
                  | {error, Reason},
          Stage :: atom(),
          Reason :: term().
relationship_process({Command, Requester, Origin, Target, Category}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    RetVal =
        case get_relationship_stage_and_properties(Origin, Target,
                                                   Category, State) of
            {error, Reason} ->
                {error, Reason};
            {CurrentStage, Properties} ->
                {RelationshipProperties, NewReqester} =
                    case CurrentStage of
                        empty ->
                            {make_relation_proplist(
                               list_to_binary(Requester),
                               list_to_binary(Origin),
                               list_to_binary(Target),
                               atom_to_binary(CurrentStage, latin1)),
                             list_to_binary(Requester)};
                        _ -> % when properties comes from db, they are binary
                             % so when comparing Requester with them, shall
                             % also be binary
                            {Properties, list_to_binary(Requester)}
                    end,
%                ?LOGFORMAT(info,"Req:~p~nCurrent:~p",[R,C]),
                Admins = State#rls_state.admins,
                case get_next_stage(Category, Command, CurrentStage,
                                    State#rls_state.logic,
                                    NewReqester,
                                    RelationshipProperties,
                                    Admins) of
                    {error, Reason} ->
                        {error, Reason};
                    NextStage ->
%                        ?LOGFORMAT(info,"Req:~p~nCurrent:~p~nNext:~p",[R,C,NextStage]),
                        InterimProplist =
                            lists:keyreplace(?RLS_RELATION_PROPERTY_STAGE,
                                             1,
                                             RelationshipProperties,
                                             {?RLS_RELATION_PROPERTY_STAGE,
                                              atom_to_list(NextStage)}),
                        NewProplist =
                            lists:map(fun({Key, Value}) ->
                                              {Key, to_list(Value)}
                                      end,
                                      InterimProplist),
                        case rls_db:update_staged_relationship(
                               Origin,
                               Target,
                               Category,
                               Type,
                               CurrentStage,
                               NewProplist,
                               State#rls_state.db_connection) of
                            empty ->
%                                ?LOGFORMAT(info,"To be deleted:~p~nProps:~p",[R,RelationshipProperties]),
                                delete_relationship(RelationshipProperties,
                                                    Origin,
                                                    Target,
                                                    Category,
                                                    Type,
                                                    State);
                            Else ->
%                                ?LOGFORMAT(info,"New Stage for ~p is ~p",[R, Else]),
                                Else
                        end
                end
        end,
    {RetVal, State}.

%% Quick fix atttempt at handling relation prop values.
to_list(Value) when is_integer(Value) ->
    Value;
to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) ->
    binary_to_list(Value).



delete_relationship(RelationshipProperties,
                    Origin,
                    Target,
                    Category,
                    Type,
                    State) ->
    case binary_to_list(
           proplists:get_value(
             ?RLS_RELATION_PROPERTY_ORIGIN,
             RelationshipProperties)) of
        Origin ->
            rls_db:delete_staged_relationship(
              Origin, Target, Category,
              State#rls_state.db_connection);
        Target ->
            case Type of
                one_way ->
                    {error, wrong_direction};
                two_way ->
                    rls_db:delete_staged_relationship(
                      Target, Origin, Category,
                      State#rls_state.db_connection)
            end
    end.

get_relationship_stage_and_properties(Origin, Target, Category, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    case rls_db:get_relationship_properties(Origin, Target, Category, Type,
                                            State#rls_state.db_connection) of
        {error, notfound} ->
            {empty, []};
        {error, Reason} ->
            {error, Reason};
        RelationshipProperties ->
            {list_to_atom(binary_to_list(
                           proplists:get_value(?RLS_RELATION_PROPERTY_STAGE,
                                               RelationshipProperties,
                                               undef))),
             RelationshipProperties}
    end.

relationship_stage({_Requester, Origin, Target, Category}, State) ->
%% if < R18
    Type = map_find(type, map_find(Category, State#rls_state.logic)),
%% when >= R18
%    #{Category := #{ type := Type}} = State#rls_state.logic,
    RetVal =
        case rls_db:get_relationship_properties(Origin, Target, Category, Type,
                                                State#rls_state.db_connection) of
            {error, Reason} ->
                {error, Reason};
            RelationshipProperties ->
                list_to_atom(binary_to_list(
                               proplists:get_value(?RLS_RELATION_PROPERTY_STAGE,
                                                   RelationshipProperties,
                                                   undef)))
        end,
    {RetVal, State}.

relationship_check_stage({Origin, Target, Category, Stage}, State) ->
    case relationship_stage({Origin, Origin, Target, Category}, State) of
     {Stage, NewState} ->
         {true, NewState};
     {_, NewState} ->
         {false, NewState}
    end.

relationship_commands({_Requester, _Origin, _Target, Category} = Req, State) ->
    {Value, NewState} = relationship_stage(Req, State),
    Stage =
        case Value of
            {error, notfound} ->
                empty;
            {error, Reason} ->
                {error, Reason};
            Else ->
                Else
        end,
    RetVal =
        case Stage of
            {error, Reason2} ->
                {error, Reason2};
            RetStage ->
%% if < R18
                Commands =
                    map_find(RetStage,
                             map_find(mapping,
                                      map_find(Category,
                                               State#rls_state.logic))),
%% when >= R18
%    #{Category := #{ mapping := #{ RetStage := Commands}}} =
%                    NewState#rls_state.logic,
                maps:keys(Commands)
        end,
    {RetVal, NewState}.

get_relationship_targets_for_stage({Stage, UUID, Category, StartName, PageSize},
                                   State) ->
    {rls_db:get_staged_relationships(Stage,
                                     UUID,
                                     Category,
                                     StartName,
                                     PageSize,
                                     target,
                                     State#rls_state.db_connection), State}.

get_relationship_origins_for_stage({Stage, UUID, Category, StartName, PageSize},
                                   State) ->
    {rls_db:get_staged_relationships(Stage,
                                     UUID,
                                     Category,
                                     StartName,
                                     PageSize,
                                     origin,
                                     State#rls_state.db_connection), State}.

get_relationships_for_stage({Stage, UUID, Category, StartName, PageSize},
                                   State) ->
    {rls_db:get_staged_relationships(Stage,
                                     UUID,
                                     Category,
                                     StartName,
                                     PageSize,
                                     undefined,
                                     State#rls_state.db_connection), State}.

handle_call({Request, Args}, _From, State) ->
    {Reply, NewState} = ?MODULE:Request(Args, State),
    {reply, Reply, NewState};

handle_call(Request, _From, State) ->
    ?LOGFORMAT(warning, "Unhandled request:~p for csi_service with state:~p~n",
               [Request, State]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_follower_entities_with_offset({UUID, Offset, Limit}, State) ->
    {rls_db:get_follower_entities_with_offset(UUID,
                                              Offset,
                                              Limit,
                                              ?RLS_RELATIONSHIP_FOLLOW,
                                              State#rls_state.db_connection),
     State}.

get_followed_entities_with_offset({UUID, Offset, Limit}, State) ->
    {rls_db:get_followed_entities_with_offset(UUID,
                                              Offset,
                                              Limit,
                                              ?RLS_RELATIONSHIP_FOLLOW,
                                              State#rls_state.db_connection),
     State}.

get_followed_last_posts_with_offset({UUID, Offset}, State) ->
    {rls_db:get_followed_last_posts_with_offset(
       UUID,
       Offset,
       0,
       ?RLS_DEFAULT_PAGE_SIZE,
       ?RLS_RELATIONSHIP_FOLLOW,
       State#rls_state.db_connection),
     State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ====================================================================
%% Internal functions
%% ====================================================================
get_next_stage(Category, Command, Stage, Logic, Requester, Proplist, Admins) ->
    try
%% if < R18
        Tuple =
            map_find(Command,
                      map_find(Stage,
                                map_find(mapping,
                                          map_find(Category,
                                                    Logic)))),
%% when >= R18
%    #{Category := #{ mapping := #{ Stage := #{Command := Tuple}}}} = Logic,
%%         ?LOGFORMAT(info,"Next stage: ~p->~p~nCmd:~p, Req:~p, Props:~p",
%%                    [Stage, Tuple, Command, Requester, Proplist]),
        case Tuple of
            {NextStage, any} ->
                NextStage;
            {NextStage, ValidInitiator} ->
                check_if_valid_next_stage(NextStage, ValidInitiator,
                                          Requester, Proplist, Admins)
        end
    catch
        _:_ ->
            ?LOGFORMAT(error,
                       "Not matching. Category:~p Stage:~p Command:~p "
                       "with Logic:~p Requester:~p Proplist:~p",
                       [Category, Stage, Command, Logic,
                        Requester, Proplist]),
            {error, invalid_command}
    end.

check_if_valid_next_stage(NextStage, ValidInitiator, Requester, Proplist, Admins) ->
    Initiator = case ValidInitiator of
                    target ->
                        proplists:get_value(?RLS_RELATION_PROPERTY_TARGET,
                                            Proplist, undefined);
                    origin ->
                        proplists:get_value(?RLS_RELATION_PROPERTY_ORIGIN,
                                            Proplist, undefined);
                    prev_requester ->
                        proplists:get_value(?RLS_RELATION_PROPERTY_STAGED_BY,
                                            Proplist, undefined);
                    admin ->
                        case lists:member(binary_to_list(Requester), Admins) of
                            true ->
                                Requester;
                            false ->
                                <<"">>
                        end
                end,
    case Requester of
        Initiator ->
            NextStage;
        _ ->
            {error, bad_requester}
    end.

make_relation_proplist(Requester, Origin, Target, Stage) ->
    [{?RLS_RELATION_PROPERTY_STAGED_BY, Requester},
     {?RLS_RELATION_PROPERTY_ORIGIN, Origin},
     {?RLS_RELATION_PROPERTY_TARGET, Target},
     {?RLS_RELATION_PROPERTY_STAGE, Stage}].

map_find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        _Else ->
            throw("Key not found")
    end.
