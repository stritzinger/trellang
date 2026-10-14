-module(trello_health_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
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
    trello_test_fixture:init_per_suite(Config).

end_per_suite(Config) ->
    trello_test_fixture:end_per_suite(Config).

init_per_testcase(Case, Config) ->
    trello_test_fixture:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    trello_test_fixture:end_per_testcase(Case, Config).

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
