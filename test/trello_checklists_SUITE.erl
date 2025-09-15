-module(trello_checklists_SUITE).

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([list_empty_on_new_card/1, create_checklist_on_card/1, rename_and_reorder_checklist/1, check_item_crud/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [list_empty_on_new_card,
     create_checklist_on_card,
     rename_and_reorder_checklist,
     check_item_crud].

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

check_item_crud(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello:create_card(ListId, #{name => <<"checkitems-crud">>}),
            register_cleanup_card(CardId),
            {ok, CL} = trello:create_checklist(CardId, <<"Todos">>),
            CLId = maps:get(<<"id">>, CL),
            {ok, ItemA} = trello:add_check_item(CLId, <<"one">>, #{}),
            IAId = maps:get(<<"id">>, ItemA),
            {ok, ItemB} = trello:add_check_item(CLId, <<"two">>, #{pos => <<"top">>}),
            IBId = maps:get(<<"id">>, ItemB),
            {ok, ItemB2} = trello:rename_check_item(CardId, IBId, <<"two-renamed">>),
            <<"two-renamed">> = maps:get(<<"name">>, ItemB2),
            {ok, ItemB3} = trello:set_check_item_state(CardId, IBId, complete),
            <<"complete">> = maps:get(<<"state">>, ItemB3),
            {ok, _ItemB4} = trello:set_check_item_pos(CardId, IBId, <<"bottom">>),
            ok = trello:delete_check_item(CLId, IAId),
            ok
    end.

rename_and_reorder_checklist(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello:create_card(ListId, #{name => <<"checklists-rename">>}),
            register_cleanup_card(CardId),
            {ok, CL0} = trello:create_checklist(CardId, <<"Initial">>),
            CLId = maps:get(<<"id">>, CL0),
            {ok, CL1} = trello:rename_checklist(CLId, <<"Renamed">>),
            <<"Renamed">> = maps:get(<<"name">>, CL1),
            {ok, _CL2} = trello:set_checklist_pos(CLId, <<"top">>),
            {ok, Lists} = trello:list_checklists(CardId),
            true = is_list(Lists),
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


