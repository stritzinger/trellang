-module(trello_cards_read_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([read_existing_card/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [read_existing_card].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

read_existing_card(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            %% Strategy: create a minimal card first, then fetch it via get_card/1
            {ok, CardCreated} = trello:create_card(#{name => <<"CT read test">>}),
            CardId = maps:get(<<"id">>, CardCreated),
            {ok, Card} = trello:get_card(CardId),
            _ = maps:get(<<"id">>, Card),
            _ = maps:get(<<"name">>, Card),
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



