-module(trello_lists_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([list_lists_on_board/1, list_cards_for_list/1, dump_board_structure/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [list_lists_on_board, list_cards_for_list, dump_board_structure].

init_per_suite(Config) ->
    trello_test_fixture:init_per_suite(Config).

end_per_suite(Config) ->
    trello_test_fixture:end_per_suite(Config).

init_per_testcase(Case, Config) ->
    trello_test_fixture:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    trello_test_fixture:end_per_testcase(Case, Config).

list_lists_on_board(_Config) ->
    case application:get_env(trellang, board_id) of
        undefined -> {skip, "missing trellang.board_id in dev.config"};
        {ok, BoardId} ->
            {ok, Lists} = trello:list_lists(BoardId),
            true = is_list(Lists),
            %% verify list objects carry id and name
            case Lists of
                [L | _] ->
                    _ = maps:get(<<"id">>, L),
                    _ = maps:get(<<"name">>, L),
                    ok;
                [] -> ct:fail(no_lists)
            end
    end.

list_cards_for_list(Config) ->
    case {application:get_env(trellang, list_id)} of
        {undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {{ok, ListId}} ->
            %% create a card and ensure listing finds it
            {ok, Card0} = trello_test_fixture:create_card(Config, ListId, #{name => <<"CT list cards test">>}),
            CardId = maps:get(<<"id">>, Card0),
            {ok, Cards} = trello:list_cards_for_list(ListId, #{fields => <<"id,name,idList">>, limit => 1000}),
            true = lists:any(fun(#{<<"id">> := Id}) -> Id =:= CardId; (_) -> false end, Cards),
            ok
    end.

dump_board_structure(_Config) ->
    case application:get_env(trellang, board_id) of
        undefined -> {skip, "missing trellang.board_id in dev.config"};
        {ok, BoardId} ->
            {ok, Dump} = trello:dump_board(BoardId, #{}),
            Board = maps:get(board, Dump),
            _ = maps:get(<<"id">>, Board),
            _ = maps:get(<<"name">>, Board),
            Lists = maps:get(lists, Dump),
            true = is_list(Lists),
            %% Also exercise include_checklists option path for coverage
            {ok, Dump2} = trello:dump_board(BoardId, #{include_checklists => true}),
            _ = maps:get(board, Dump2),
            _ = maps:get(lists, Dump2),
            ok
    end.
