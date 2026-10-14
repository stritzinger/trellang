-module(trello_cards_read_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([read_existing_card/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [read_existing_card].

init_per_suite(Config) ->
    trello_test_fixture:init_per_suite(Config).

end_per_suite(Config) ->
    trello_test_fixture:end_per_suite(Config).

init_per_testcase(Case, Config) ->
    trello_test_fixture:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    trello_test_fixture:end_per_testcase(Case, Config).

read_existing_card(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            %% Strategy: create a minimal card first, then fetch it via get_card/1
            {ok, ListId0} = application:get_env(trellang, list_id),
            {ok, CardCreated} = trello_test_fixture:create_card(Config, ListId0, #{name => <<"CT read test">>}),
            CardId = maps:get(<<"id">>, CardCreated),
            {ok, Card} = trello:get_card(CardId),
            _ = maps:get(<<"id">>, Card),
            _ = maps:get(<<"name">>, Card),
            ok
    end.
