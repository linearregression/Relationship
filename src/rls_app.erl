%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% relationship application
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(rls_app).
-behaviour(application).

-include("rls_common.hrl").

-export([start/2]).
-export([stop/1]).
-export([debug/0]).

debug() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(idna),
    ok = application:start(jiffy),
    ok = application:start(mimerl),
    ok = application:start(certifi),
    ok = application:start(hackney),
    ok = application:start(neo4j),
    ok = application:start(csi),
    ok = application:start(rls),
    ?LOGMSG(debug, "Application rls started").


start(_Type, _Args) ->
    rls_sup:start_link().

stop(_State) ->
    ok.
