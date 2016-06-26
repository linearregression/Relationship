%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% rls common constants
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-define(RLS_SERVICE_MODULE,rls_service).

-define(RLS_TIMESTAMP_PROPERTY_NAME, "last_posted_timestamp").
-define(RLS_POSTID_PROPERTY_NAME, "last_post_id").
-define(RLS_FULLNAME_PROPERTY_NAME, "entity_full_name").
-define(RLS_FULLNAME_UUID_PROPERTY_NAME, "full_name_UUID").
-define(RLS_UUID_PROPERTY_NAME, "UUID").

-define(RLS_TIMESTAMP_PROPERTY_NAME_BIN,<<"last_posted_timestamp">>).
-define(RLS_POSTID_PROPERTY_NAME_BIN,<<"last_post_id">>).
-define(RLS_FULLNAME_PROPERTY_NAME_BIN,<<"entity_full_name">>).
-define(RLS_FULLNAME_UUID_PROPERTY_NAME_BIN,<<"full_name_UUID">>),
-define(RLS_UUID_PROPERTY_NAME_BIN,<<"UUID">>).

-define(RLS_MAKE_FULLNAME_UUID(FullName, UUID), FullName ++ " " ++ UUID).
-define(RLS_RELATION_PROPERTY_TARGET,<<"Target">>).
-define(RLS_RELATION_PROPERTY_ORIGIN,<<"Origin">>).
-define(RLS_RELATION_PROPERTY_STAGED_BY,<<"StagedBy">>).
-define(RLS_RELATION_PROPERTY_STAGE,<<"Stage">>).

-define(RLS_NODE_LABEL_ENTITY,"Entity").
-define(RLS_NODE_LABEL_SPORT,"Sport").
-define(RLS_NODE_LABEL_LEAGUE,"League").
-define(RLS_NODE_LABEL_TEAM,"Team").
-define(RLS_NODE_LABEL_USER,"User").

%% @TODO these shall be put into a config file
-define(RLS_DEFAULT_PAGE_SIZE,25).

-ifndef(LOGFORMAT).

-ifdef(lager).
-compile([{parse_transform, lager_transform}]).
-define(LOGTYPE,"lager").
-define(LOGFORMAT(Level,Format,Args),
        _ = lager:Level(Format,Args)).
-define(LOGMSG(Level,Format),
        _ = lager:Level(Format)).

-else.
-define(LOGTYPE,"io:format").
-define(LOGFORMAT(Level, Format, Args),
        ok = io:format("~p: ~s~n",[Level,io_lib:format(Format, Args)])).
-define(LOGMSG(Level,Format),
        ok = io:format("~p: ~p~n",[Level,Format])).
-endif.
-endif.

-type(property_list() :: list(proplists:property())).

-type offset()          :: non_neg_integer().
-type page_size()       :: integer().
-type post_id()         :: binary(). % GUID returned when post is created
-type entity_name()     :: binary(). %%% previous note: string()
-type rls_timestamp()   :: non_neg_integer(). % microseconds since epoch
-type uuid()            :: binary().

-type entity()          :: {entity_name(), uuid()}.
-type post()            :: {uuid(), post_id(), rls_timestamp()}.
-type start_name_uuid() :: {string(), string()}.
