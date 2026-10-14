-module(trello_checklists_SUITE).

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([list_empty_on_new_card/1, create_checklist_on_card/1, rename_and_reorder_checklist/1, check_item_crud/1, check_item_due_and_assign/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [list_empty_on_new_card,
     create_checklist_on_card,
     rename_and_reorder_checklist,
     check_item_crud,
     check_item_due_and_assign].

suite() -> [{timetrap, {seconds, 90}}].

init_per_suite(Config) ->
    trello_test_fixture:init_per_suite(Config).

end_per_suite(Config) ->
    trello_test_fixture:end_per_suite(Config).

init_per_testcase(Case, Config) ->
    trello_test_fixture:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    trello_test_fixture:end_per_testcase(Case, Config).

list_empty_on_new_card(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello_test_fixture:create_card(Config, ListId, #{name => <<"checklists-baseline">>}),
            {ok, Checklists} = trello:list_checklists(CardId),
            true = is_list(Checklists),
            [] = Checklists,
            ok
    end.

check_item_due_and_assign(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, BoardId} = application:get_env(trellang, board_id),
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello_test_fixture:create_card(Config, ListId, #{name => <<"checkitems-due-assign">>}),
            {ok, CL} = trello:create_checklist(CardId, <<"Todos">>),
            CLId = maps:get(<<"id">>, CL),
            {ok, Item} = trello:add_check_item(CLId, <<"due-assign">>, #{}),
            ItId = maps:get(<<"id">>, Item),
            {ok, Names} = trello:list_board_usernames(BoardId),
            case Names of
                [U | _] ->
                    {ok, MemberId} = trello:get_member_id_by_username(BoardId, U),
                    Due = <<"2031-01-02T03:04:05.000Z">>,
                    {ok, _} = trello:set_check_item_due(CardId, ItId, Due),
                    {ok, _} = trello:assign_check_item_member(CardId, ItId, MemberId),
                    {ok, _} = trello:assign_check_item_member(CardId, ItId, undefined),
                    {ok, _} = trello:set_check_item_due(CardId, ItId, null),
                    ok;
                _ -> {skip, "no member on board"}
            end
    end.

check_item_crud(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello_test_fixture:create_card(Config, ListId, #{name => <<"checkitems-crud">>}),
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

rename_and_reorder_checklist(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello_test_fixture:create_card(Config, ListId, #{name => <<"checklists-rename">>}),
            {ok, CL0} = trello:create_checklist(CardId, <<"Initial">>),
            CLId = maps:get(<<"id">>, CL0),
            {ok, CL1} = trello:rename_checklist(CLId, <<"Renamed">>),
            <<"Renamed">> = maps:get(<<"name">>, CL1),
            {ok, _CL2} = trello:set_checklist_pos(CLId, <<"top">>),
            {ok, Lists} = trello:list_checklists(CardId),
            true = is_list(Lists),
            ok
    end.

create_checklist_on_card(Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId} = application:get_env(trellang, list_id),
            {ok, #{<<"id">> := CardId}} = trello_test_fixture:create_card(Config, ListId, #{name => <<"checklists-create">>}),
            {ok, CL} = trello:create_checklist(CardId, <<"Todo">>),
            _ = maps:get(<<"id">>, CL),
            CardId = maps:get(<<"idCard">>, CL),
            <<"Todo">> = maps:get(<<"name">>, CL),
            {ok, All} = trello:list_checklists(CardId),
            true = lists:any(fun(#{<<"id">> := Id}) -> Id =:= maps:get(<<"id">>, CL); (_) -> false end, All),
            ok
    end.
