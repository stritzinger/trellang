-module(trello_health_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    health_check/1
]).

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [health_check].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) ->
    ok.

health_check(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined} -> {skip, "missing trellang.trello_token in dev.config"};
        {_, _} ->
            {ok, Me} = trello:me(),
            %% basic fields we expect from Trello /members/me
            _ = maps:get(<<"id">>, Me),
            _ = maps:get(<<"username">>, Me),
            ok
    end.

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> error(Reason)
    end.

ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, Reason} -> error(Reason)
    end.


