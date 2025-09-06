-module(trello_cards_update_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([update_name_and_desc/1, update_real_due_date/1, update_label_by_color/1, update_member_by_username/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [update_name_and_desc, update_real_due_date, update_label_by_color, update_member_by_username].

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
            {ok, ListId0} = application:get_env(trellang, list_id),
            {ok, Card0} = trello:create_card(ListId0, #{name => <<"CT update test">>}),
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

update_member_by_username(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, BoardId0} = application:get_env(trellang, board_id),
            case trello:list_board_usernames(BoardId0) of
                {ok, [Username | _]} ->
                    {ok, MemberId} = trello:get_member_id_by_username(BoardId0, Username),
                    {ok, ListId0} = application:get_env(trellang, list_id),
                    {ok, Card0} = trello:create_card(ListId0, #{name => <<"CT member username test">>}),
                    CardId = maps:get(<<"id">>, Card0),
                    {ok, _} = trello:add_member(CardId, MemberId),
                    ok = wait_for_member(CardId, MemberId, 60),
                    ok;
                _ -> {skip, "no members returned for board"}
            end
    end.

update_label_by_color(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, BoardId0} = application:get_env(trellang, board_id),
            case find_any_label_id_on_board(BoardId0) of
                {error, not_found} -> {skip, "no standard color label found on configured board"};
                {ok, LabelId} ->
                    {ok, ListId0} = application:get_env(trellang, list_id),
                    {ok, Card0} = trello:create_card(ListId0, #{name => <<"CT label color test">>}),
                    CardId = maps:get(<<"id">>, Card0),
                    %% use add_label helper to ensure idLabels is returned afterwards
                    {ok, _} = trello:add_label(CardId, LabelId),
                    ok = wait_for_label(CardId, LabelId, 60),
                    ok
            end
    end.


to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).

wait_for_member(CardId, MemberId, Attempts) ->
    {ok, Members} = trello:get_card_members(CardId),
    IdMembers = [ maps:get(<<"id">>, M) || M <- Members ],
    case lists:member(MemberId, IdMembers) of
        true -> ok;
        false when Attempts =< 0 -> ct:fail({member_not_visible, MemberId});
        false -> timer:sleep(1000), wait_for_member(CardId, MemberId, Attempts - 1)
    end.

wait_for_label(CardId, LabelId, Attempts) ->
    {ok, Labels} = trello:get_card_labels(CardId),
    Has = lists:any(
            fun(#{<<"id">> := Id}) -> Id =:= LabelId;
               (_) -> false
            end, Labels),
    case Has of
        true -> ok;
        false when Attempts =< 0 -> ct:fail({label_not_visible, LabelId});
        false -> timer:sleep(1000), wait_for_label(CardId, LabelId, Attempts - 1)
    end.

find_any_label_id_on_board(BoardId0) ->
    Colors = [<<"green">>, <<"blue">>, <<"yellow">>, <<"red">>],
    find_first_ok(BoardId0, Colors).

find_first_ok(BoardId0, [C | Rest]) ->
    case trello:get_label_id_by_color(BoardId0, C) of
        {ok, LabelId} -> {ok, LabelId};
        {error, not_found} -> find_first_ok(BoardId0, Rest);
        {error, _} -> find_first_ok(BoardId0, Rest)
    end;
find_first_ok(_BoardId0, []) -> {error, not_found}.

update_real_due_date(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            {ok, ListId0} = application:get_env(trellang, list_id),
            {ok, Card0} = trello:create_card(ListId0, #{name => <<"CT due test">>}),
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


