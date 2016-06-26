%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% relationship service API
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(rls).

-include("rls_common.hrl").
-include("rls.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([create_entity/2,
         create_entity/1,
         follow_entity/2,
         unfollow_entity/2,
         update_last_post_timestamp/3,
         get_followed_last_posts/1,
         get_followed_last_posts/2,
         get_followed_last_posts/3,
         get_followed_last_posts/4,
         get_followed_last_posts_p/4,
         nr_of_followed_entities/1,
         get_followed_entities/1,
         get_followed_entities/2,
         get_followed_entities/3,
         nr_of_follower_entities/1,
         get_follower_entities/1,
         get_follower_entities/2,
         get_follower_entities/3,
         set_entity_property/3,
         get_entity_property/2,
         get_entity_properties/1,
         delete_entity_property/2,
         set_relationship_property/5,
         get_relationship_property/4,
         get_relationship_properties/3,
         get_relationship_properties/4,
         delete_relationship_property/4,
         delete_entity/1]).

-export([logic_clear/0,
         logic_add/1,
         logic_get/0,
         logic_get/1,
         relationship_process/5,
         relationship_commands/4,
         relationship_stage/4,
         relationship_check/3,
         relationship_check_stage/4,
         get_relationship_targets_count/2,
         get_relationship_targets/2,
         get_relationship_targets/3,
         get_relationship_targets/4,
         get_relationship_targets_for_stage/3,
         get_relationship_targets_for_stage/4,
         get_relationship_targets_for_stage/5,
         get_relationship_origins_count/2,
         get_relationship_origins/2,
         get_relationship_origins/3,
         get_relationship_origins/4,
         get_relationship_origins_for_stage/3,
         get_relationship_origins_for_stage/4,
         get_relationship_origins_for_stage/5,
         get_active_relationships/2,
         get_active_relationships/3,
         get_active_relationships/4
        ]).

-export([list_macros/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([get_followed_entities_with_offset/2,
         get_followed_entities_with_offset/3,
         get_follower_entities_with_offset/2,
         get_follower_entities_with_offset/3,
         get_followed_last_posts_with_offset/2
        ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-export([get_indexes/0]).

start() -> csi:start(?RLS_SERVICE_NAME, ?RLS_SERVICE_MODULE).
start_link() -> csi:start_link(?RLS_SERVICE_NAME, ?RLS_SERVICE_MODULE).

stop() -> csi:stop(?RLS_SERVICE_NAME).

%% create_entity/1
%% ====================================================================
%% @doc Creates an entity node and sets the FullName property to ""
%% @end
-spec create_entity(UUID :: uuid()) -> Result when
          Result :: property_list()
                  | {error, Reason},
          Reason :: property_list().
create_entity(UUID) ->
    create_entity(UUID, " ").

%% create_entity/2
%% ====================================================================
%% @doc Creates an entity node and sets the FullName property
%% @end
-spec create_entity(UUID :: uuid(), FullName :: string()) ->
          Result when
          Result :: property_list()
                  | {error, Reason},
          Reason :: property_list().
create_entity(UUID, FullName) ->
    csi:call_p(?RLS_SERVICE_NAME,
               create_entity,
               {UUID, FullName}
              ).
%% delete_entity/1
%% ====================================================================
%% @doc Deletes an entity node
%% @end
-spec delete_entity(UUID :: uuid()) ->
          Result when
          Result :: ok
              | {error, Reason},
          Reason :: term().
delete_entity(UUID) -> csi:call_p(?RLS_SERVICE_NAME,
                                  delete_entity,
                                  UUID).
%% get_entity_properties/1
%% ====================================================================
%% @doc Returns the property list of the given entity or notfound
%% @end
-spec get_entity_properties(UUID :: uuid()) ->
          Result when
          Result :: property_list()
                  | notfound
                  | {error, Reason},
          Reason :: term().
get_entity_properties(UUID) -> csi:call_p(?RLS_SERVICE_NAME,
                                          get_entity_properties,
                                          UUID).

%% update_last_post_timestamp/2
%% ====================================================================
%% @doc Update and entity's last posted timestamp and postid
%% @end
-spec update_last_post_timestamp(UUID, TimeStamp, PostId) ->
          Result when
          UUID :: uuid(),
          TimeStamp :: erlang:timestamp(),
          PostId :: post_id(),
          Result :: ok
                    | {error, Reason},
          Reason :: term().
update_last_post_timestamp(UUID, TimeStamp, PostId) ->
    csi:call_p(?RLS_SERVICE_NAME,
               update_last_post_timestamp,
               {UUID, TimeStamp, PostId}).

%% follow_entity/2
%% ====================================================================
%% @doc Sets a non unique relation named Follows
%% @end
-spec follow_entity(UUID         :: uuid(),
                    FollowerUUID :: uuid()) ->
          Result when
          Result :: ok
                  | {error, Reason},
          Reason :: term().
follow_entity(UUID, FollowerUUID) ->
    case relationship_process(create,
                              FollowerUUID,
                              FollowerUUID,
                              UUID,
                              ?RLS_RELATIONSHIP_FOLLOW) of
        active ->
            ok;
        Else ->
            Else
    end.

%% unfollow_entity/2
%% ====================================================================
%% @doc Deletes all relation that named as Follows
%% @end
-spec unfollow_entity(UUID          :: uuid(),
                      FollowerUUID  :: uuid()) ->
          Result when
          Result :: ok
                  | {error, Reason},
          Reason :: term().
unfollow_entity(UUID, FollowerUUID) ->
        case relationship_process(delete,
                              FollowerUUID,
                              FollowerUUID,
                              UUID,
                              ?RLS_RELATIONSHIP_FOLLOW) of
        empty ->
            ok;
        Else ->
            Else
    end.

%% get_followed_last_posts/1
%% ====================================================================
%% @doc Returns the ordered list of post (max 25) IDs of followed entities
%% @end
-spec get_followed_last_posts(UUID) ->
          Result when
          UUID      :: uuid(),
          Result    :: [] | [post()].
get_followed_last_posts(UUID) ->
    get_followed_last_posts(UUID, now, 0, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_last_posts/2
%% ====================================================================
%% @doc Returns the ordered list of post (max 25) IDs of followed entities
%% @end
-spec get_followed_last_posts(UUID, LastPostedTimestamp) ->
          Result when
          UUID                :: uuid(),
          LastPostedTimestamp :: rls_timestamp(),
          Result              :: [] | [post()].
get_followed_last_posts(UUID, LastPostedTimestamp) ->
    get_followed_last_posts(UUID, now, LastPostedTimestamp, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_last_posts/3
%% ====================================================================
%% @doc Returns the ordered list of post (max 25) IDs of followed entities
%% @end
-spec get_followed_last_posts(UUID, FromTime, LastPostedTimestamp) ->
          Result when
          UUID                :: uuid(),
          FromTime            :: now | rls_timestamp(),
          LastPostedTimestamp :: rls_timestamp(),
          Result              :: [] | [post()].
get_followed_last_posts(UUID, FromTime, LastPostedTimestamp) ->
    get_followed_last_posts(UUID, FromTime, LastPostedTimestamp, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_last_posts/4
%% ====================================================================
%% @doc Returns the ordered list of post IDs of followed entities
%% @end
-spec get_followed_last_posts(UUID, FromTime, LastPostedTimestamp, PageSize) ->
          Result when
          UUID                :: uuid(),
          FromTime            :: now | rls_timestamp(),
          LastPostedTimestamp :: rls_timestamp(),
          PageSize            :: page_size(),
          Result              :: [] | [post()] | {error, Reason},
          Reason              :: term().
get_followed_last_posts(UUID, FromTime, LastPostedTimestamp, PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_followed_last_posts,
               {UUID, FromTime, LastPostedTimestamp, PageSize}).

%% get_followed_last_posts_p/4
%% ====================================================================
%% @doc Returns the ordered list of post IDs of followed entities
%% @end
-spec get_followed_last_posts_p(UUID, FromTime, LastPostedTimestamp, PageSize) ->
          Result when
          UUID                :: uuid(),
          FromTime            :: now | rls_timestamp(),
          LastPostedTimestamp :: rls_timestamp(),
          PageSize            :: page_size(),
          Result              :: [] | [post()] | {error, Reason},
          Reason              :: term().
get_followed_last_posts_p(UUID, FromTime, LastPostedTimestamp, PageSize) ->
    csi:cast_p(?RLS_SERVICE_NAME,
               get_followed_last_posts,
               {UUID, FromTime, LastPostedTimestamp, PageSize}).

%% nr_of_follower_entities/1
%% ====================================================================
%% @doc Returns the number of followers for an entity
%% @end
-spec nr_of_follower_entities(UUID) ->
          Result when
          UUID   :: uuid(),
          Result :: integer().
nr_of_follower_entities(UUID) ->
    get_relationship_origins_count(UUID, ?RLS_RELATIONSHIP_FOLLOW).

%% get_follower_entities/1
%% ====================================================================
%% @doc Returns the ordered list of followers' (max 25) Name and UUIDs
%% @end
-spec get_follower_entities(UUID) ->
          Result when
          UUID   :: uuid(),
          Result :: [] | [entity()].
get_follower_entities(UUID) ->
    get_follower_entities(UUID, {" ", " "}, ?RLS_DEFAULT_PAGE_SIZE).

%% get_follower_entities/2
%% ====================================================================
%% @doc Returns the ordered list of followers' (max 25) Name and UUIDs
%% @end
-spec get_follower_entities(UUID, StartNameUUID) ->
          Result when
          UUID :: uuid(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_follower_entities(UUID, StartNameUUID) ->
    get_follower_entities(UUID, StartNameUUID, ?RLS_DEFAULT_PAGE_SIZE).

%% get_follower_entities/3
%% ====================================================================
%% @doc Returns the ordered list of followers' (max 25) Name and UUIDs
%% @end
-spec get_follower_entities(UUID, StartNameUUID, PageSize) ->
          Result when
          UUID :: uuid(),
          StartNameUUID :: start_name_uuid(),
          PageSize :: page_size(),
          Result :: [] | [entity()].
get_follower_entities(UUID, StartNameUUID, PageSize) ->
    get_relationship_origins(UUID,
                             ?RLS_RELATIONSHIP_FOLLOW,
                             StartNameUUID,
                             PageSize).

%% nr_of_followed/1
%% ====================================================================
%% @doc Returns the number of followed entity for an entity
%% @end
nr_of_followed_entities(UUID) ->
    get_relationship_targets_count(UUID, ?RLS_RELATIONSHIP_FOLLOW).

%% get_followed_entities/1
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_followed_entities(UUID) ->
          Result when
          UUID :: uuid(),
          Result :: [] | [entity()].
get_followed_entities(UUID) ->
    get_followed_entities(UUID, {" ", " "}, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_entities/2
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_followed_entities(UUID, StartNameUUID) ->
          Result when
          UUID :: uuid(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_followed_entities(UUID, StartNameUUID) ->
    get_followed_entities(UUID, StartNameUUID, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_entities/3
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_followed_entities(UUID, StartNameUUID, PageSize) ->
          Result when
          UUID :: uuid(),
          StartNameUUID :: start_name_uuid(),
          PageSize :: page_size(),
          Result :: [] | [entity()].
get_followed_entities(UUID, StartNameUUID, PageSize) ->
    get_relationship_targets(UUID,
                             ?RLS_RELATIONSHIP_FOLLOW,
                             StartNameUUID,
                             PageSize).

%% set_relationship_property/5
%% ====================================================================
%% @doc Set a property for an edge
%% @end
-spec set_relationship_property(Origin, Target, Category, Property, Value) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          Value :: term(),
          Result :: notfound
                    | property_list().
set_relationship_property(Origin, Target, Category, Property, Value) ->
    csi:call_p(?RLS_SERVICE_NAME,
               set_relationship_property,
               {Origin, Target, rls_utils:fix_case(Category), Property, Value}).

%% get_relationship_property/4
%% ====================================================================
%% @doc Get a property for an edge
%% @end
-spec get_relationship_property(Origin, Target, Category, Properties) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Properties :: string(),
          Result :: null            % in case the proprty is missing
                    | notfound      % in case the entity is missing
                    | term().
get_relationship_property(Origin, Target, Category, Properties) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_property,
               {Origin, Target, rls_utils:fix_case(Category), Properties}).

%% get_relationship_properties/3
%% ====================================================================
%% @doc Get all properties for an edge
%% @end
-spec get_relationship_properties(Origin, Target, Category) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Result :: null            % in case the proprty is missing
                    | notfound      % in case the entity is missing
                    | list(term()).
get_relationship_properties(Origin, Target, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_properties,
               {Origin, Target, rls_utils:fix_case(Category)}).

%% get_relationship_properties/4
%% ====================================================================
%% @doc Get all properties for an edge
%% @end
-spec get_relationship_properties(Origin, Target, Category, Properties) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Properties :: list(string()),
          Result :: null            % in case all proprtes are missing
                    | notfound      % in case the entity is missing
                    | list(term()).
get_relationship_properties(Origin, Target, Category, Properties) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_specific_properties,
               {Origin, Target, rls_utils:fix_case(Category), Properties}).

%% delete_relationship_property/4
%% ====================================================================
%% @doc Delete a property for an edge
%% @end
-spec delete_relationship_property(Origin, Target, Category, Property) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: string(),
          Property :: string(),
          Result :: notfound
                    | property_list().
delete_relationship_property(Origin, Target, Category, Property) ->
    csi:call_p(?RLS_SERVICE_NAME,
               delete_relationship_property,
               {Origin, Target, rls_utils:fix_case(Category), Property}).

%% get_relationship_targets/2
%% ====================================================================
%% @doc Returns the ordered list of entities that are target for the
%% Category. (max 25)
%% @end
-spec get_relationship_targets(UUID, Category) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          Result :: [] | [entity()].
get_relationship_targets(UUID, Category) ->
    get_relationship_targets(UUID, Category, {" ", " "}, ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_targets/3
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_targets(UUID, Category, StartNameUUID) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_targets(UUID, Category, StartNameUUID) ->
    get_relationship_targets(UUID, Category, StartNameUUID,
                             ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_targets/4
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_targets(UUID, Category, StartNameUUID, PageSize) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          PageSize :: page_size(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_targets(UUID, Category, StartNameUUID, PageSize) ->
    get_relationship_targets_for_stage(active,
                                       UUID,
                                       Category,
                                       StartNameUUID,
                                       PageSize).

%% get_relationship_targets_for_stage/3
%% ====================================================================
%% @doc Returns the ordered list of entities that are target for the
%% Category. (max 25)
%% @end
-spec get_relationship_targets_for_stage(Stage, UUID, Category) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          Result :: [] | [entity()].
get_relationship_targets_for_stage(Stage, UUID, Category) ->
    get_relationship_targets_for_stage(Stage, UUID, Category, {" ", " "},
                                       ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_targets_for_stage/4
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_targets_for_stage(Stage, UUID, Category,
                                         StartNameUUID) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_targets_for_stage(Stage, UUID, Category, StartNameUUID) ->
    get_relationship_targets_for_stage(Stage, UUID, Category, StartNameUUID,
                                       ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_targets_for_stage/5
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_targets_for_stage(Stage,
                                         UUID,
                                         Category,
                                         StartNameUUID,
                                         PageSize) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          PageSize :: page_size(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_targets_for_stage(Stage, UUID, Category, StartNameUUID,
                                   PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_targets_for_stage,
               {Stage, UUID, rls_utils:fix_case(Category), StartNameUUID, PageSize}).

%% get_relationship_origins/2
%% ====================================================================
%% @doc Returns the ordered list of entities that are target for the
%% Category. (max 25)
%% @end
-spec get_relationship_origins(UUID, Category) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          Result :: [] | [entity()].
get_relationship_origins(UUID, Category) ->
    get_relationship_origins(UUID, Category, {" ", " "}, ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_origins/3
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_origins(UUID, Category, StartNameUUID) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_origins(UUID, Category, StartNameUUID) ->
    get_relationship_origins(UUID, Category, StartNameUUID,
                             ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_origins/4
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_origins(UUID, Category, StartNameUUID, PageSize) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          PageSize :: page_size(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_origins(UUID, Category, StartNameUUID, PageSize) ->
    get_relationship_origins_for_stage(active, UUID, Category,
                                       StartNameUUID, PageSize).
%% get_relationship_origins_for_stage/3
%% ====================================================================
%% @doc Returns the ordered list of entities that are target for the
%% Category. (max 25)
%% @end
-spec get_relationship_origins_for_stage(Stage, UUID, Category) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          Result :: [] | [entity()].
get_relationship_origins_for_stage(Stage, UUID, Category) ->
    get_relationship_origins_for_stage(Stage, UUID, Category, {" ", " "},
                                       ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_origins_for_stage/4
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_origins_for_stage(Stage, UUID, Category,
                                         StartNameUUID) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_origins_for_stage(Stage, UUID, Category, StartNameUUID) ->
    get_relationship_origins_for_stage(Stage, UUID, Category, StartNameUUID,
                                       ?RLS_DEFAULT_PAGE_SIZE).

%% get_relationship_origins_for_stage/5
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_relationship_origins_for_stage(Stage,
                                         UUID,
                                         Category,
                                         StartNameUUID,
                                         PageSize) ->
          Result when
          Stage :: atom(),
          UUID :: uuid(),
          Category :: string(),
          PageSize :: page_size(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_relationship_origins_for_stage(Stage, UUID, Category, StartNameUUID,
                                   PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_origins_for_stage,
               {Stage, UUID, rls_utils:fix_case(Category), StartNameUUID, PageSize}).

%% get_relationship_origins_count/2
%% ====================================================================
%% @doc Returns the nr of origin entity pointing to entity in active
%% stage
%% @end
-spec get_relationship_origins_count(UUID, Category) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          Result :: non_neg_integer()
                  | {error, notfound}.
get_relationship_origins_count(UUID, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_origins_count,
               {UUID, rls_utils:fix_case(Category)}).

%% get_relationship_targets_count/2
%% ====================================================================
%% @doc Returns the nr of origin entity pointing to entity in active
%% stage
%% @end
-spec get_relationship_targets_count(UUID, Category) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          Result :: non_neg_integer()
                  | {error, notfound}.
get_relationship_targets_count(UUID, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationship_targets_count,
               {UUID, rls_utils:fix_case(Category)}).


%% get_active_relationships/2
%% ====================================================================
%% @doc Returns the ordered list of entities that are target for the
%% Category. (max 25)
%% @end
-spec get_active_relationships(UUID, Category) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          Result :: [] | [entity()].
get_active_relationships(UUID, Category) ->
    get_active_relationships(UUID, Category, {" ", " "}, ?RLS_DEFAULT_PAGE_SIZE).

%% get_active_relationships/3
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_active_relationships(UUID, Category, StartNameUUID) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_active_relationships(UUID, Category, StartNameUUID) ->
    get_active_relationships(UUID, Category, StartNameUUID,
                             ?RLS_DEFAULT_PAGE_SIZE).

%% get_active_relationships/4
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_active_relationships(UUID, Category, StartNameUUID, PageSize) ->
          Result when
          UUID :: uuid(),
          Category :: string(),
          PageSize :: page_size(),
          StartNameUUID :: start_name_uuid(),
          Result :: [] | [entity()].
get_active_relationships(UUID, Category, StartNameUUID, PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_relationships_for_stage,
               {active, UUID, rls_utils:fix_case(Category), StartNameUUID, PageSize}).

%% set_entity_property/3
%% ====================================================================
%% @doc Set a property for an entiy
%% @end
-spec set_entity_property(UUID, Property, Value) ->
          Result when
          UUID :: uuid(),
          Property :: string(),
          Value :: term(),
          Result :: notfound
                    | property_list().
set_entity_property(UUID, Property, Value) ->
    csi:call_p(?RLS_SERVICE_NAME,
               set_entity_property,
               {UUID, Property, Value}).

%% get_entity_property/2
%% ====================================================================
%% @doc Get a property for an entiy
%% @end
-spec get_entity_property(UUID, Property) ->
          Result when
          UUID :: uuid(),
          Property :: string(),
          Result :: null            % in case the proprty is missing
                    | notfound      % in case the entity is missing
                    | term().
get_entity_property(UUID, Property) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_entity_property,
               {UUID, Property}).

%% delete_entity_property/2
%% ====================================================================
%% @doc Delete a property for an entiy
%% @end
-spec delete_entity_property(UUID, Property) ->
          Result when
          UUID :: uuid(),
          Property :: string(),
          Result :: notfound
                    | property_list().
delete_entity_property(UUID, Property) ->
    csi:call_p(?RLS_SERVICE_NAME,
               delete_entity_property,
               {UUID, Property}).

logic_clear() ->
    csi:call(?RLS_SERVICE_NAME, logic_clear, []).

logic_add(Logic) ->
    csi:call(?RLS_SERVICE_NAME, logic_add, Logic).

logic_get() ->
    csi:call(?RLS_SERVICE_NAME, logic_get, []).

logic_get(LogicName) ->
    csi:call(?RLS_SERVICE_NAME, logic_get, LogicName).

%% relationship_process/5
%% ====================================================================
%% @doc make a state change based on the command
%% @end
-spec relationship_process(Command, Requester, Origin, Target, Type) ->
          Result when
          Command :: atom(),
          Requester :: string(),
          Origin :: string(),
          Target :: string(),
          Type :: term(),
          Result :: Stage
                  | {error, Reason},
          Stage :: atom(),
          Reason :: term().
relationship_process(Command, Requester, Origin, Target, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               relationship_process,
               {Command, Requester, Origin, Target, rls_utils:fix_case(Category)}).

%% relationship_stage/4
%% ====================================================================
%% @doc return a stage of a relationship
%% @end
-spec relationship_stage(Requester, Origin, Target, Category) ->
          Result when
          Requester :: string(),
          Origin :: string(),
          Target :: string(),
          Category :: term(),
          Result :: Stage
                  | {error, Reason},
          Stage :: atom(),
          Reason :: term().
relationship_stage(Requester, Origin, Target, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               relationship_stage,
               {Requester, Origin, Target, rls_utils:fix_case(Category)}).

%% relationship_check/3
%% ====================================================================
%% @doc return a boolean true if a relationship is active
%% @end
-spec relationship_check(Origin, Target, Category) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: term(),
          Result :: boolean().
relationship_check(Origin, Target, Category) ->
    relationship_check_stage(Origin, Target, Category, active).

%% relationship_check_stage/4
%% ====================================================================
%% @doc check a stage of a relationship
%% @end
-spec relationship_check_stage(Origin, Target, Category, Stage) ->
          Result when
          Origin :: string(),
          Target :: string(),
          Category :: term(),
          Stage :: atom(),
          Result :: boolean().
relationship_check_stage(Origin, Target, Category, Stage) ->
    csi:call_p(?RLS_SERVICE_NAME,
               relationship_check_stage,
               {Origin, Target, rls_utils:fix_case(Category), Stage}).

%% relationship_commands/4
%% ====================================================================
%% @doc return the possible command for a relationship in its stage
%% @end
-spec relationship_commands(Requester, Origin, Target, Category) ->
          Result when
          Requester :: string(),
          Origin :: string(),
          Target :: string(),
          Category :: term(),
          Result :: {ok, [Command]}
                  | {ok, []}
                  | {error, Reason},
          Command :: atom(),
          Reason :: term().
relationship_commands(Requester, Origin, Target, Category) ->
    csi:call_p(?RLS_SERVICE_NAME,
               relationship_commands,
               {Requester, Origin, Target, rls_utils:fix_case(Category)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove it from clean V2. These functions are here to support
%% V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get_followed_entities_with_offset/2
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_followed_entities_with_offset(UUID, Offset) ->
          Result when
          UUID      :: uuid(),
          Offset    :: offset(),
          Result    :: [] | [entity()].
get_followed_entities_with_offset(UUID, Offset) ->
    get_followed_entities_with_offset(UUID, Offset, ?RLS_DEFAULT_PAGE_SIZE).

%% get_followed_entities_with_offset/3
%% ====================================================================
%% @doc Returns the ordered list of followeds' (max 25) Name and UUIDs
%% @end
-spec get_followed_entities_with_offset(UUID, Offset, PageSize) ->
          Result when
          UUID      :: uuid(),
          Offset    :: offset(),
          PageSize  :: page_size(),
          Result    :: [] | [entity()].
get_followed_entities_with_offset(UUID, Offset, PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_followed_entities_with_offset,
               {UUID, Offset, PageSize}).

%% get_follower_entities_with_offset/2
%% ====================================================================
%% @doc Returns the ordered list of followers' (max 25) Name and UUIDs
%% @end
-spec get_follower_entities_with_offset(UUID, Offset) ->
          Result when
          UUID      :: uuid(),
          Offset    :: offset(),
          Result    :: [] | [entity()].
get_follower_entities_with_offset(UUID, Offset) ->
    get_follower_entities_with_offset(UUID, Offset, ?RLS_DEFAULT_PAGE_SIZE).

%% get_follower_entities_with_offset/3
%% ====================================================================
%% @doc Returns the ordered list of followers' (max 25) Name and UUIDs
%% @end
-spec get_follower_entities_with_offset(UUID, Offset, PageSize) ->
          Result when
          UUID      :: uuid(),
          Offset    :: offset(),
          PageSize  :: page_size(),
          Result    :: [] | [entity()].
get_follower_entities_with_offset(UUID, Offset, PageSize) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_follower_entities_with_offset,
               {UUID, Offset, PageSize}).

%% get_followed_last_posts_with_offset/2
%% ====================================================================
%% @doc Returns the ordered list of post (max 25) IDs of followed entities
%% @end
-spec get_followed_last_posts_with_offset(UUID, Offset) ->
          Result when
          UUID      :: uuid(),
          Offset    :: offset(),
          Result    :: [] | [post()].
get_followed_last_posts_with_offset(UUID, Offset) ->
    csi:call_p(?RLS_SERVICE_NAME,
               get_followed_last_posts_with_offset,
               {UUID, Offset}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @TODO remove the above from clean V2. These functions are here to
%% support V1 only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_macros() ->
    ?LOGFORMAT(info,
               "LOGTYPE:~p~n",
               [?LOGTYPE
               ]
    ).

%% Housekeeping functions

%% get_indexes() -> csi:call(?RLS_SERVICE_NAME,
%%                           get_node_index_properties,
%%                           []).
