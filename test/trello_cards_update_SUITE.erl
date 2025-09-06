-module(trello_cards_update_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([update_name_and_desc/1, update_real_due_date/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [update_name_and_desc, update_real_due_date].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

update_name_and_desc(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, Card0} = trello:create_card(#{name => <<"CT update test">>}),
            CardId = maps:get(<<"id">>, Card0),
            {ok, Updated} = trello:update_card(CardId, #{name => <<"Updated name">>, desc => <<"Updated desc">>, due => <<"null">>, pos => <<"top">>}),
            %% verify echoed fields
            <<"Updated name">> = maps:get(<<"name">>, Updated),
            <<"Updated desc">> = maps:get(<<"desc">>, Updated),
            null = maps:get(<<"due">>, Updated),
            PosVal = maps:get(<<"pos">>, Updated),
            true = is_number(PosVal),
            %% fetch again to be sure persisted
            {ok, Card} = trello:get_card(CardId),
            <<"Updated name">> = maps:get(<<"name">>, Card),
            <<"Updated desc">> = maps:get(<<"desc">>, Card),
            ok
    end.

update_real_due_date(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, Card0} = trello:create_card(#{name => <<"CT due test">>}),
            CardId = maps:get(<<"id">>, Card0),
            Due = <<"2030-01-02T03:04:05.000Z">>,
            {ok, Updated} = trello:update_card(CardId, #{due => Due}),
            Due = maps:get(<<"due">>, Updated),
            {ok, Card} = trello:get_card(CardId),
            Due = maps:get(<<"due">>, Card),
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


