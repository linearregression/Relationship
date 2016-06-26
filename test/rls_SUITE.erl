%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(rls_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

-include_lib("common_test/include/ct.hrl").

-include("../include/rls.hrl").

-define(UUID_PREFIX, "SQOR").
-define(MAKE_UUID(Number), ?UUID_PREFIX ++ integer_to_list(Number)).
-define(MAKE_FULL_NAME(Number), (?UUID_PREFIX ++ " " ++ integer_to_list(Number))).
-define(NR_OF_TEST_NODES, 5).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 5000}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    setup_db(Config),
  {ok, _Apps} = application:ensure_all_started(rls),
%%    csi:stats_exclude_funs(?RLS_SERVICE_NAME, all),
    case setup_rls_logic(Config) of
        false ->
            {skip, logicmissing};
        TruthySetupResult ->
            ct:print(info, "All Apps Started ~p", [TruthySetupResult]),
            case rls:nr_of_followed_entities(?MAKE_UUID(?NR_OF_TEST_NODES)) of
                ?NR_OF_TEST_NODES - 1 ->
                    Config;
                {error, Reason} ->
                    ct:print(error, "Neo4j is unavailable. Reason:~p", [Reason]),
                    {skip, neo4junavailable};
                HigherEntityCount ->
                    ct:print(info,
                             "Creating data as it was ~p~n"
                             "It may take several minutes!!",
                             [HigherEntityCount]),
                    create_nodes(?NR_OF_TEST_NODES),
                    update_random_timestamp(?NR_OF_TEST_NODES - 1,
                                            ?NR_OF_TEST_NODES div 2),
                    update_random_timestamp(1, 1),
                    Config
            end
    end.


%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config, Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ct:print(info, "Statistics (collection is set in line 71 of rsl_SUITE.erl):"
             "~n~p", [csi:stats_get_all(?RLS_SERVICE_NAME)]),
%    delete_nodes(200),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config, Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config, Config1} | {fail, Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType, N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip, Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [entity_create_delete,
     entity_properties,
     relationship_properties,
     follow_test,
     last_posts,
     relationship_logic,
     relationship_process_follow,
     relationship_process_chat,
     relationship_process_friend,
     relationship_check,
     relationship_admin_client,
     last_posts_check,
     add_entites_test,
     add_follow_test
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Test case info function - returns list of tuples to set
%%              properties for the test case.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%--------------------------------------------------------------------
my_test_case() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip, Reason} | {comment, Comment} |
%%               {save_config, Config1} | {skip_and_save, Reason, Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------

entity_create_delete(_Config) ->
    match([{<<"full_name_UUID">>, <<"XTEST 1 XTEST1">>},
           {<<"UUID">>, <<"XTEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"XTEST 1">>}],
          rls:create_entity("XTEST1", "XTEST 1")),
    ok = rls:delete_entity("XTEST1").

entity_properties(_Config) ->
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:create_entity("TEST1", "TEST 1")),
    <<"TEST 1">> =
        rls:get_entity_property("TEST1", "entity_full_name"),
    match([{<<"last_posted_timestamp">>,0},
          {<<"full_name_UUID">>,<<"  TEST2">>},
          {<<"entity_full_name">>,<<" ">>},
          {<<"UUID">>,<<"TEST2">>}],
          rls:create_entity("TEST2")),
    <<" ">> = rls:get_entity_property("TEST2", "entity_full_name"),
    ok = rls:delete_entity("TEST2"),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"NewProperty">>, <<"NewProperty Value">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:set_entity_property("TEST1", "NewProperty", "NewProperty Value")),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"NewProperty">>, <<"NewProperty Value">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:get_entity_properties("TEST1")),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:delete_entity_property("TEST1", "NewProperty")),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:get_entity_properties("TEST1")),
    ok = rls:delete_entity("TEST1").

relationship_properties(_Config) ->
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:create_entity("TEST1", "TEST 1")),
    match([{<<"full_name_UUID">>, <<"TEST 2 TEST2">>},
           {<<"UUID">>, <<"TEST2">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 2">>}],
          rls:create_entity("TEST2", "TEST 2")),
    active = rls:relationship_process(create, "TEST1", "TEST1", "TEST2",
                                      ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} = rls:get_relationship_properties("TEST2", "TEST1",
                                                        ?RLS_RELATIONSHIP_FOLLOW
                                                       ),
    match([{<<"Origin">>, <<"TEST1">>},
           {<<"Target">>, <<"TEST2">>},
           {<<"StagedBy">>, <<"TEST1">>},
           {<<"Stage">>, <<"active">>}],
          rls:get_relationship_properties("TEST1", "TEST2",
                                          ?RLS_RELATIONSHIP_FOLLOW)),
    match([{<<"Origin">>, <<"TEST1">>},
           {<<"Stage">>, <<"active">>}],
          rls:get_relationship_properties("TEST1", "TEST2",
                                          ?RLS_RELATIONSHIP_FOLLOW,
                                          ["Origin",
                                           "Stage"]
                                         )),
    <<"active">> = rls:get_relationship_property("TEST1", "TEST2",
                                                 ?RLS_RELATIONSHIP_FOLLOW, "Stage"),
    match([{<<"Origin">>, <<"TEST1">>},
           {<<"Target">>, <<"TEST2">>},
           {<<"StagedBy">>, <<"TEST1">>},
           {<<"Stage">>, <<"active">>},
           {<<"NewProperty">>, <<"New Value">>}],
          rls:set_relationship_property("TEST1", "TEST2",
                                        ?RLS_RELATIONSHIP_FOLLOW,
                                        "NewProperty", "New Value")),
    match([{<<"Origin">>, <<"TEST1">>},
           {<<"Target">>, <<"TEST2">>},
           {<<"StagedBy">>, <<"TEST1">>},
           {<<"Stage">>, <<"active">>},
           {<<"NewProperty">>, 42}],
          rls:set_relationship_property("TEST1", "TEST2",
                                        ?RLS_RELATIONSHIP_FOLLOW,
                                        "NewProperty", 42)),
    match([{<<"Origin">>, <<"TEST1">>},
           {<<"Target">>, <<"TEST2">>},
           {<<"StagedBy">>, <<"TEST1">>},
           {<<"Stage">>, <<"active">>}],
          rls:delete_relationship_property("TEST1", "TEST2",
                                           ?RLS_RELATIONSHIP_FOLLOW,
                                           "NewProperty")),
    ok = rls:delete_entity("TEST1"),
    ok = rls:delete_entity("TEST2").

follow_test(_Config) ->
    ?NR_OF_TEST_NODES - 1 =
        rls:nr_of_follower_entities("SQOR1"),
    ?NR_OF_TEST_NODES - 1 =
        rls:nr_of_followed_entities(?MAKE_UUID(?NR_OF_TEST_NODES)),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_followed_entities("SQOR3"),
    ok =
        rls:follow_entity("SQOR3", "SQOR2"),
    {error, notfound} =
        rls:follow_entity("SQORX1", "SQOR3"),
    {error, notfound} =
        rls:follow_entity("SQOR1", "SQORX3"),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_followed_entities("SQOR3"),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 3">>, <<"SQOR3">>}] =
        rls:get_followed_entities("SQOR2"),
    {error, bad_requester} =
        rls:relationship_process(delete, "SQOR3", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    ok =
        rls:unfollow_entity("SQOR3", "SQOR2"),
    [{<<"SQOR 1">>, <<"SQOR1">>}] =
        rls:get_followed_entities("SQOR2"),
    ok =
        rls:follow_entity("SQOR2", "SQOR3"),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_followed_entities("SQOR3"),
    A =
    [{list_to_binary(?MAKE_FULL_NAME(?NR_OF_TEST_NODES-1)),
      list_to_binary(?MAKE_UUID(?NR_OF_TEST_NODES-1))},
     {list_to_binary(?MAKE_FULL_NAME(?NR_OF_TEST_NODES)),
      list_to_binary(?MAKE_UUID(?NR_OF_TEST_NODES))}],
    A =
        rls:get_follower_entities(?MAKE_UUID(?NR_OF_TEST_NODES - 2)).

%% add_entites_test() ->
%%     start(),
%%     UUIDEntity = <<"123">>, % old format based on relational DB
%%     UUIDFollower = <<"456">>, % old format based on relational DB
%%     Entity = rls:create_entity(UUIDEntity),
%%     Follower = rls:create_entity(UUIDFollower),
%%     ?assertEqual([{<<"UUID">>,<<"123">>}], Entity),
%%     ?assertEqual([{<<"UUID">>,<<"456">>}], Follower),
%%     {[{<<"id">>,_Id}, {<<"type">>,Relationship}]} =
%%         rls:follow_entity(UUIDEntity, UUIDFollower),
%%     ?assert(<<"FOLLOWS">> =:= Relationship).
%% 
%% add_follow__test() ->
%%     start(),
%%     UUIDEntity = <<"123">>, % old format based on relational DB
%%     UUIDFollower = <<"456">>, % old format based on relational DB
%% 
%%     ?assertEqual(true, rls:unfollow_entity(UUIDEntity, UUIDFollower)).

add_entites_test(_Config) ->
    UUIDEntity = "12300", % old format based on relational DB
    UUIDFollower = "45600", % old format based on relational DB
    Entity = rls:create_entity(UUIDEntity),
    Follower = rls:create_entity(UUIDFollower),
    ct:pal("Add entity follow:~p->~p. Result:~p",
           [Entity,
            Follower,
            rls:follow_entity(UUIDEntity, UUIDFollower)]).

add_follow_test(_Config) ->
    UUIDEntity = "12300", % old format based on relational DB
    UUIDFollower = "45600", % old format based on relational DB
    A = rls:unfollow_entity(UUIDEntity, UUIDFollower),
    ct:pal("Unfollow:~p", [A]),
    rls:delete_entity(UUIDEntity),
    rls:delete_entity(UUIDFollower).

last_posts(_Config) ->
    [] =
        rls:get_followed_last_posts("SQOR1"),
    _ =
        rls:delete_entity_property(?MAKE_UUID(?NR_OF_TEST_NODES), "last_post_id"),
    null =
        rls:get_entity_property(?MAKE_UUID(?NR_OF_TEST_NODES), "last_post_id").
%%     A = sets:from_list([{<<"full_name_UUID">>, <<"SQOR 1 SQOR1">>},
%%          {<<"last_post_id">>, <<"PostedId">>},
%%          {<<"UUID">>, <<"SQOR1">>},
%%          {<<"last_posted_timestamp">>, _},
%%          {<<"entity_full_name">>, <<"SQOR 1">>}]),
%%     A = sets:from_list(rls:update_last_post_timestamp("SQOR1",
%%                                                       os:timestamp(),
%%                                                       "PostedId")).

relationship_logic(Config) ->
    #{} = rls:logic_clear(),
%% Follows' commands:
%%      create, delete, block, hide, unblock, show
%% stages:
%%      empty, active, blocked, hidden, hidden_blocked
    Logic1 = #{?RLS_RELATIONSHIP_FOLLOW =>
                   #{ type => one_way,
                      mapping =>
                          #{ empty =>
                                 #{ create  => {active, origin}},
                             active =>
                                 #{ delete  => {empty, origin},
                                    block   => {blocked, target},
                                    hide    => {hidden, origin}},
                             blocked =>
                                 #{ delete  => {empty, origin},
                                    unblock => {active, target}},
                             hidden =>
                                 #{ show    => {active, origin},
                                    delete  => {empty, any},
                                    block   => {blocked, target}}}}},
    Logic1 = rls:logic_add(Logic1),
    %% Friend' commands:
%%      create, delete, request, accept, cancel, block, decline, hide, show,
%% stages:
%%      empty, pending, active, blocked, muted
    Logic2 = #{?RLS_RELATIONSHIP_FRIEND =>
                   #{ type => two_way,
                      mapping =>
                          #{ empty =>
                                 #{ create  => {pending, any}},
                             pending =>
                                 #{ accept  => {active, target},
                                    cancel  => {empty, origin},
                                    decline => {empty, target},
                                    block   => {blocked, target},
                                    delete  => {empty, any}},
                             active =>
                                 #{ delete  => {empty, any},
                                    block   => {blocked, any}},
                             blocked =>
                                 #{ unblock => {active, prev_requester},
                                    delete  => {empty, any}}}}},
    NewMap = maps:merge(Logic1, Logic2),
    NewMap = rls:logic_add(Logic2),
    NewMap = rls:logic_get(),
    MapValue = maps:get(?RLS_RELATIONSHIP_FOLLOW, Logic1),
    MapValue = rls:logic_get(?RLS_RELATIONSHIP_FOLLOW),
    LogicChat = #{?RLS_RELATIONSHIP_CHAT =>
                   #{ type => two_way,
                      mapping =>
                          #{ empty =>
                                 #{ create => {active, any}},
                             active =>
                                 #{ delete => {empty, any}}}}},
    _ = rls:logic_add(LogicChat),
    #{} = rls:logic_clear(),
    setup_rls_logic(Config),
    ok.

relationship_process_follow(_Config) ->
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_followed_entities("SQOR3"),
    ok =
        rls:unfollow_entity("SQOR2", "SQOR3"),
    [{<<"SQOR 1">>, <<"SQOR1">>}] =
        rls:get_followed_entities("SQOR3"),
    ok =
        rls:follow_entity("SQOR2", "SQOR3"),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_followed_entities("SQOR3"),
    {error, bad_requester} =
        rls:relationship_process(create, "SQORX1", "SQOR3", "SQORX1", ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} =
        rls:relationship_process(create, "SQORX1", "SQORX1", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} =
        rls:relationship_process(create, "SQOR1", "SQOR1", "SQORX3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, same_entity} =
        rls:relationship_process(create, "SQOR3", "SQOR3", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    active =
        rls:relationship_process(create, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    [{<<"SQOR 1">>, <<"SQOR1">>},
     {<<"SQOR 3">>, <<"SQOR3">>}] =
        rls:get_followed_entities("SQOR2"),
    active =
        rls:relationship_stage("SQORADMIN", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} =
        rls:relationship_stage("SQORADMIN", "SQOR1", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    [block, create, delete, hide] =
        rls:relationship_commands("SQOR1", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SOQR2X", "SOQR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SOQR2", "SOQR3X", ?RLS_RELATIONSHIP_FOLLOW),
    hidden =
        rls:relationship_process(hide, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    hidden =
        rls:relationship_stage("SQORADMIN", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    active =
        rls:relationship_stage("SQORADMIN", "SQOR3", "SQOR2", ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} =
        rls:relationship_stage("SQORADMIN", "SQOR1", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SQOR3", "SQOR2", ?RLS_RELATIONSHIP_FOLLOW),
    active =
        rls:relationship_process(show, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    active =
        rls:relationship_stage("SQORADMIN", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    ok =
        rls:relationship_process(delete, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    {error, notfound} =
        rls:relationship_stage("SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_FOLLOW),
    [{<<"SQOR 1">>, <<"SQOR1">>}] =
        rls:get_followed_entities("SQOR2"),
    ok.

relationship_process_chat(_Config) ->
    {error, invalid_command} =
        rls:relationship_process(delete, "SQOR1", "SQOR1", "SQOR4", ?RLS_RELATIONSHIP_CHAT),
    {error, notfound} =
        rls:relationship_process(create, "SQORX1", "SQORX1", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, notfound} =
        rls:relationship_process(create, "SQOR1", "SQOR1", "SQORX3", ?RLS_RELATIONSHIP_CHAT),
    {error, same_entity} =
        rls:relationship_process(create, "SQOR3", "SQOR3", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    _ =
        rls:relationship_process(delete, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    _ =
        rls:relationship_process(create, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    active =
        rls:relationship_process(accept, "SQOR3", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    [{<<"SQOR 3">>, <<"SQOR3">>}] =
        rls:get_relationship_targets("SQOR2", ?RLS_RELATIONSHIP_CHAT),
    [{<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_relationship_origins("SQOR3", ?RLS_RELATIONSHIP_CHAT),
    [{<<"SQOR 3">>, <<"SQOR3">>}] =
        rls:get_active_relationships("SQOR2", ?RLS_RELATIONSHIP_CHAT),
    [{<<"SQOR 2">>, <<"SQOR2">>}] =
        rls:get_active_relationships("SQOR3", ?RLS_RELATIONSHIP_CHAT),
    active =
        rls:relationship_stage("SQORADMIN", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, notfound} =
        rls:relationship_stage("SQORADMIN", "SQOR1", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    [accept, block, create, delete] =
        rls:relationship_commands("SQOR1", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SOQR2X", "SOQR3", ?RLS_RELATIONSHIP_CHAT),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SOQR2", "SOQR3X", ?RLS_RELATIONSHIP_CHAT),
    {error, notfound} =
        rls:relationship_stage("SQORADMIN", "SQOR1", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, invalid_command} =
        rls:relationship_process(unblock, "SQOR2", "SQOR3", "SQOR2", ?RLS_RELATIONSHIP_CHAT),
    ok =
        rls:relationship_process(delete, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, notfound} =
        rls:relationship_stage("SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    {error, invalid_command} =
        rls:relationship_process(delete, "SQOR2", "SQOR2", "SQOR3", ?RLS_RELATIONSHIP_CHAT),
    [{<<"SQOR 1">>, <<"SQOR1">>}] =
        rls:get_followed_entities("SQOR2"),
    ok.

relationship_check(_Config) ->
    true =
        rls:relationship_check("SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    false =
        rls:relationship_check("SQOR1", "SQOR2", ?RLS_RELATIONSHIP_FOLLOW),
    false =
        rls:relationship_check("SQOR2", "SQOR1", ?RLS_RELATIONSHIP_CHAT),
    false =
        rls:relationship_check("SQORZ2", "SQOR1", ?RLS_RELATIONSHIP_CHAT),
    hidden =
        rls:relationship_process(hide, "SQOR2", "SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    false =
        rls:relationship_check("SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    ?NR_OF_TEST_NODES - 1 -1 =
        rls:nr_of_follower_entities("SQOR1"),
    false =
        rls:relationship_check("SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    active =
        rls:relationship_process(show, "SQOR2", "SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    ?NR_OF_TEST_NODES - 1 =
        rls:nr_of_follower_entities("SQOR1"),
    true =
        rls:relationship_check("SQOR2", "SQOR1", ?RLS_RELATIONSHIP_FOLLOW),
    ok.

last_posts_check(_Config) ->
    ok.

relationship_process_friend(_Config) ->
    rls:delete_entity("TEST1"),
    rls:delete_entity("TEST2"),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:create_entity("TEST1", "TEST 1")),
    match([{<<"full_name_UUID">>, <<"  TEST2">>},
           {<<"UUID">>, <<"TEST2">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<" ">>}],
          rls:create_entity("TEST2")),
    {error, notfound} =
        rls:relationship_stage("SQORAdmin", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    [create] =
        rls:relationship_commands("SQORAdmin", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    {error, invalid_command} =
        rls:relationship_process(accept, "TEST2", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    [create] =
        rls:relationship_commands("TEST1", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    [create] =
        rls:relationship_commands("TEST1", "TEST1", "TEST2", "fRiEnds"),
    pending =
        rls:relationship_process(create, "TEST1", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    {error, bad_requester} =
        rls:relationship_process(accept, "TEST1", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    active =
        rls:relationship_process(accept, "TEST2", "TEST2", "TEST1", ?RLS_RELATIONSHIP_FRIEND),
    blocked =
        rls:relationship_process(block, "TEST1", "TEST2", "TEST1", ?RLS_RELATIONSHIP_FRIEND),
    {error, bad_requester} =
        rls:relationship_process(unblock, "TEST2", "TEST1", "TEST2", ?RLS_RELATIONSHIP_FRIEND),
    active =
        rls:relationship_process(unblock, "TEST1", "TEST2", "TEST1", ?RLS_RELATIONSHIP_FRIEND),
    ok =
        rls:relationship_process(delete, "TEST2", "TEST2", "TEST1", ?RLS_RELATIONSHIP_FRIEND),
    ok =
        rls:delete_entity("TEST1"),
    ok =
        rls:delete_entity("TEST2"),
    ok.

relationship_admin_client(_Config) ->
    rls:delete_entity("TEST1"),
    rls:delete_entity("TEST2"),
    match([{<<"full_name_UUID">>, <<"TEST 1 TEST1">>},
           {<<"UUID">>, <<"TEST1">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<"TEST 1">>}],
          rls:create_entity("TEST1", "TEST 1")),
    match([{<<"full_name_UUID">>, <<"  TEST2">>},
           {<<"UUID">>, <<"TEST2">>},
           {<<"last_posted_timestamp">>, 0},
           {<<"entity_full_name">>, <<" ">>}],
          rls:create_entity("TEST2")),
    {error, notfound} =
        rls:relationship_stage("SQORAdmin", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    [create] =
        rls:relationship_commands("SQORAdmin", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    {error, invalid_command} =
        rls:relationship_process(accept, "SQORADMIN", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    [create] =
        rls:relationship_commands("SQORADMIN", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    active =
        rls:relationship_process(create, "SQORADMIN", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    {error, bad_requester} =
        rls:relationship_process(delete, "TEST1", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    {error, invalid_command} =
        rls:relationship_process(delete, "SQORADMIN", "TEST2", "TEST1", ?RLS_RELATIONSHIP_CLIENT),
    ok =
        rls:relationship_process(delete, "SQORADMIN", "TEST1", "TEST2", ?RLS_RELATIONSHIP_CLIENT),
    ok =
        rls:delete_entity("TEST1"),
    ok =
        rls:delete_entity("TEST2"),
    ok.

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
    io:format("~nCreated ~p nodes. Run time was ~p (~p)~n", [NrOfNodes,
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

create_entity(UUID) ->
    case rls:create_entity(?MAKE_UUID(UUID),
                           ?UUID_PREFIX ++ " " ++ integer_to_list(UUID)
                          ) of
        {error, Reason} ->
            ct:print(error, "Creating entity:~p", [Reason]);
        Else ->
            Else
    end.

delete_nodes(0) ->
    ok;

delete_nodes(Nr) ->
    rls:delete_entity(?MAKE_UUID(Nr)),
    delete_nodes(Nr - 1).

setup_rls_logic(Config) ->
    CFile = ?config(data_dir, Config) ++ "../../config/sys.config",
    case file:consult(CFile) of
        {ok, [Terms]} ->
            {rls, TermList} = lists:keyfind(rls, 1, Terms),
            {logic, Logic} = lists:keyfind(logic, 1, TermList),
            rls:logic_add(Logic),
            ok;
        _Else ->
            false
    end.

setup_db(Config) ->
    CFile = ?config(data_dir, Config) ++ "../../config/sys.config",
    case file:consult(CFile) of
        {ok, [Terms]} ->
            {rls, TermList} = lists:keyfind(rls, 1, Terms),
            {neo4j_connection, Neo} = lists:keyfind(neo4j_connection, 1, TermList),
            ct:pal("Using:~p", [Neo]),
            ok = application:set_env(rls,
                                     neo4j_connection,
                                     Neo,
                                     [{persistent, true}]),
            {admins, Admins} = lists:keyfind(admins, 1, TermList),
            ct:pal("Admins:~p",[Admins]),
            ok = application:set_env(rls, admins, Admins, [{persistent, true}]),
            ok;
        _Else ->
            false
    end.

match(P1, P2) ->
    try
        true = sets:is_subset(sets:from_list(P1), sets:from_list(P2)) andalso
               sets:is_subset(sets:from_list(P2), sets:from_list(P1))
    catch
        E:R ->
            ct:pal("~p:~p. Stack:~p", [E, R, erlang:get_stacktrace()])
    end.
