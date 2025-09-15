-module(trello_checklists_SUITE).

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([list_empty_on_new_card/1, create_checklist_on_card/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [list_empty_on_new_card,
     create_checklist_on_card].

suite() -> [{timetrap, {seconds, 90}}].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TC, Config) ->
    erlang:erase(cleanup_cards),
    Config.

end_per_testcase(_TC, Config) ->
    maybe_cleanup_created_cards(),
    Config.

list_empty_on_new_card(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello:create_card(ListId, #{name => <<"checklists-baseline">>}),
            register_cleanup_card(CardId),
            {ok, Checklists} = trello:list_checklists(CardId),
            true = is_list(Checklists),
            [] = Checklists,
            ok
    end.

create_checklist_on_card(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello:create_card(ListId, #{name => <<"checklists-create">>}),
            register_cleanup_card(CardId),
            {ok, CL} = trello:create_checklist(CardId, <<"Todo">>),
            _ = maps:get(<<"id">>, CL),
            CardId = maps:get(<<"idCard">>, CL),
            <<"Todo">> = maps:get(<<"name">>, CL),
            {ok, All} = trello:list_checklists(CardId),
            true = lists:any(fun(#{<<"id">> := Id}) -> Id =:= maps:get(<<"id">>, CL); (_) -> false end, All),
            ok
    end.

register_cleanup_card(CardId) ->
    Cards = erlang:get(cleanup_cards),
    case Cards of
        undefined -> erlang:put(cleanup_cards, [CardId]);
        L when is_list(L) -> erlang:put(cleanup_cards, [CardId | L])
    end,
    ok.

maybe_cleanup_created_cards() ->
    case {application:get_env(trellang, test_cleanup), erlang:get(cleanup_cards)} of
        {{ok, true}, Cards} when is_list(Cards) -> lists:foreach(fun(C) -> _ = trello:update_card(C, #{closed => true}) end, Cards), ok;
        _ -> ok
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


