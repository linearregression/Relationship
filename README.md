# SQ2_Relationship

# ALPHA VERSION USE WITH CARE

Relationship service provides functionality for storing and retrieving nodes and edges. It relies on a graph database Neo4j.

Nodes are representing Entities, edges are the connection between the Entities, called Relation.

An Entity (node) have three fields stored along with it:

1.  EntityFullName. A string() containing the Entity's public name.
2.  UUID. string(), unique Entity Id.
3.  PostTimeStamp. {PostId,TimeStamp}. The last posted post's Id and time. PostId is a binary string(), TimeStamp is an integer() in usec.

A Relationship (edge) have only one label that tells the type of the relationship. Currently only "Follows" is used. Later on, other types like "Friend" etc cab be implemented.

## Requisites

Use erlang.mk and relx.
Have neo4j installed. Visit [Neo4j Page](http://http://neo4j.com/)

Make sure the followings are set in $(NEO4J_ROOT)/config/neo4j.properties

    # Enable auto-indexing for nodes, default is false.
    node_auto_indexing=true
    
    # The node property keys to be auto-indexed, if enabled.
    node_keys_indexable=last_posted_timestamp,UUID,full_name_UUID

This will autoindex the fields described above for faster queries. If ne4j was running, restart it.
    
    $(NEO4J_ROOT)/bin/neo4j restart
    
# Build

set BUILD_ENV to one of the following values to tell the server where the db is:

    local - 127.0.0.1:7474
    localdev - neo4j-dev.sqor.com:7474
    dev - neo4j-dev.sqor.com:7474
    stage - neo4j-stage.sqor.com:7474
    prod - neo4j-prod.sqor.com:7474
    
the dependencies will be fetched according to build_deps_$(BUILD_ENV).mk in the project directory.

sys.config is generated with ./config/generate.mk from ./config/template.sys.config based on $BUILD_ENV so config values shall be set in template.sys.config.

A simple make builds the repo. Other make parameters are 'rel', 'run', 'test', 'dialyze'.

# Quick Start with an example

First, make sure Neo4j is running

    $(NEO4J_ROOT)/bin/neo4j start
    
Clone the repository, go to its directory.

    make run

You will have an erlang shell. Here are a couple of commands to show rls service.

To create 1000 nodes with SQOR1 - SQOR1000 and FullName "SQOR 1" to "SQOR 1000":

    rls_test:create_nodes(1000).
    
This command will create 1000 nodes in a way that each Entity will have a Follows relation with all entities that have smaller number in its UUID. SQOR1 will have 999 followers, SQOR 999 will have only 1. Alltogether 1000 nodes with 499500 relations. It took about 50 minutes on my McBook Pro i7 and has taken almost 300MB disk spac. 10000 nodes and 12,7M relationships takes about 10 hours and 8GB disk space.

To retrieve the data an Entity with a UUID = "SQOR1" has:

    rls:get_entity_properties("SQOR1").
    
Get the first 25 followers of entity SQOR500:

    rls:get_follower_entities("SQOR500").
    
Get the next 15 followers;

    rls:get_follower_entities_with_offset("SQOR500",25,15).
    
These lists are ordered by Full Name field.

To get the number of followers for entity SQOR500:

    rls:nr_of_follower_entities("SQOR500").
    
As the relationship service uses the [Common Service Interface](https://github.com/Amplify-Social/SQ2_CommonServiceInterface), if you are interested, you can ask the statistics:

    csi:stats_get_all(rls_service).
        
# Data handling processes

In order to have an up-to-date relationship graph, there are a couple of things need to be maintained by other services. Lets go in the order of the life-cycle of an Entity.

## Create node (Entity)

When a user creates its entity data by registering, a node shall be created in the relationship database. This is done by

    rls:create_entity(EntityId :: string(),
                      FullName :: string()) -> property_list().
                      
In return we will get all the properties for the created entity:

    rls:create_entity("TEST1","TEST 1").

    [{<<"UUID">>,<<"TEST1">>},
     {<<"last_posted_timestamp">>,0},
     {<<"entity_full_name">>,<<"TEST 1">>}]
     
If the entity is already in the database, the call does not change the old entity, it returns it's properties. The FullName field is important to be set correctly as the DB is indexed and when later on we want to fetch the follower entities for example, we would like to see the list in the order what FullName defines.

## Properties of an entity

To fetch a single property:

    rls:get_entity_property("SQOR1","entity_full_name").
    <<"SQOR 1">>

Whenever it is needed, we can fetch all the properties stored in a node for an Entity:

    rls:get_entity_properties("SQOR1").
    
    [{<<"last_post_id">>,<<"1">>},
     {<<"UUID">>,<<"SQOR1">>},
     {<<"last_posted_timestamp">>,1439801444928499},
     {<<"entity_full_name">>,<<"SQOR 1">>}]

Here we can see another property was set in the meantime ("last_post_id"). Also the "last_posted_timestamp" is set to a value other than zero. These two properties are set together with a call:

    rls:update_last_post_timestamp("SQOR1",os:timestamp(),PostId).
    
We can set any property for an Entity for future use. For example:

    rls:set_entity_property("SQOR1","NewProperty","new_value").
    [{<<"last_post_id">>,<<"11111">>},
     {<<"NewProperty">>,<<"new_value">>},
     {<<"UUID">>,<<"SQOR1">>},
     {<<"last_posted_timestamp">>,1439818574055924},
     {<<"entity_full_name">>,<<"SQOR 1">>}]
    

## Create follow relationship

To create a one way connection between two entities:

    rls:follow_entity("SQOR1","SQOR2").

This creates a relationship labeled as "Follows" and directed from SQOR2 to SQOR1.

To check followers and followed entities:

    rls:get_followed_entities("SQOR2").
    [{<<"SQOR 1">>,<<"SQOR1">>}]
    
We get a list of tuple where each tuple contains the Entity's full name and the UUID.

## Get last posts of followed entities

If we use update_last_post_timestamp every time when an entity makes a post, we can fetch the list of last posts of followed entities. This will help to construct the feed for an entity:

    (rls_server@127.0.0.1)3> rls:get_followed_last_posts("SQOR5").
    [{<<"SQOR1">>,<<"11111">>,1440155050959556},
     {<<"SQOR3">>,<<"3">>,1440150959235447},
     {<<"SQOR2">>,<<"2">>,1440150958013179},
     {<<"SQOR4">>,<<"4">>,1440150952177425}]

The lists contains tuples of entity ids, post ids and timestamps in usec, ordered by the timestamp descending, so the most recent post is on the top of the list. The list is limited for 25 elements, so if there are more to fetch, we can use the full version of get_followed_last_posts(UUID :: string(), FromTime :: non_neg_integer(), BackUntil :: non_neg_integer, PageSize :: non_neg_integer():

    (rls_server@127.0.0.1)4> rls:get_followed_last_posts("SQOR500",now,0,5).
    [{<<"SQOR1">>,<<"11111">>,1440155050959556},
     {<<"SQOR104">>,<<"104">>,1440150959621050},
     {<<"SQOR112">>,<<"112">>,1440150959603592},
     {<<"SQOR72">>,<<"72">>,1440150959576445},
     {<<"SQOR89">>,<<"89">>,1440150959561609}]
    rls:get_followed_last_posts("SQOR500",1440150959561609,0,5).
    [{<<"SQOR89">>,<<"89">>,1440150959561609},
     {<<"SQOR6">>,<<"6">>,1440150959546694},
     {<<"SQOR91">>,<<"91">>,1440150959532293},
     {<<"SQOR177">>,<<"177">>,1440150959518384},
     {<<"SQOR188">>,<<"188">>,1440150959502359}]
     
The reason of having the last post Id appearing in the second query is that there may be posts having the same time stamp, so we need to check whether the first PostId in the list is the same as the last was in the previous query.

Alternatively we can use the version of get_followed_last_posts(UUID :: string(), Offset :: non_neg_integer():

    rls:get_followed_last_posts("SQOR500",now,0,6).
    [{<<"SQOR1">>,<<"11111">>,1440155050959556},
     {<<"SQOR104">>,<<"104">>,1440150959621050},
     {<<"SQOR112">>,<<"112">>,1440150959603592},
     {<<"SQOR72">>,<<"72">>,1440150959576445},
     {<<"SQOR89">>,<<"89">>,1440150959561609},
     {<<"SQOR6">>,<<"6">>,1440150959546694}]
    (rls_server@127.0.0.1)1> rls:get_followed_last_posts_with_offset("SQOR500",5).
    [{<<"SQOR6">>,<<"6">>,1440150959546694},
     {<<"SQOR91">>,<<"91">>,1440150959532293},
     {<<"SQOR177">>,<<"177">>,1440150959518384},
     {<<"SQOR188">>,<<"188">>,1440150959502359},
     {<<"SQOR157">>,<<"157">>,1440150959480967},
     
Here we asked the first 6 posts, then we asked the same query with offset 5. This is the reason we see PostId <<"6">> made by entity <<"SQOR6">> twice. If we had asked with offset of 6, the first element in the second query would have been <<"SQOR91">>. In the example above, the post ids are the same as the entity's number.

## To be continued.

There are a number of queries implemented, please visit rls.erl for the full list. As the requirements will evolve, the service shall be extended with more and more queries for comfortable use. 