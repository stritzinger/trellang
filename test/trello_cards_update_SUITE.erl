-module(trello_cards_update_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([update_name_and_desc/1, update_real_due_date/1, update_labels_and_members/1, update_label_by_color/1, update_member_by_username/1]).

suite() -> [{timetrap, {seconds, 90}}].
all() -> [update_name_and_desc, update_real_due_date, update_labels_and_members, update_label_by_color, update_member_by_username].

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

update_member_by_username(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            case trello:list_board_usernames() of
                {ok, [Username | _]} ->
                    {ok, MemberId} = trello:get_member_id_by_username(Username),
                    {ok, Card0} = trello:create_card(#{name => <<"CT member username test">>}),
                    CardId = maps:get(<<"id">>, Card0),
                    {ok, _} = trello:add_member(CardId, MemberId),
                    {ok, Card} = trello:get_card(CardId),
                    IdMembers = maps:get(<<"idMembers">>, Card, []),
                    true = lists:member(MemberId, IdMembers),
                    ok;
                _ -> {skip, "no members returned for board"}
            end
    end.

update_label_by_color(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _} ->
            case find_any_label_id() of
                {error, not_found} -> {skip, "no standard color label found on configured board"};
                {ok, LabelId} ->
                    {ok, Card0} = trello:create_card(#{name => <<"CT label color test">>}),
                    CardId = maps:get(<<"id">>, Card0),
                    %% use add_label helper to ensure idLabels is returned afterwards
                    {ok, _} = trello:add_label(CardId, LabelId),
                    Card = wait_card_with_labels(CardId, 8),
                    Labels = maps:get(<<"labels">>, Card, []),
                    IdLabels = maps:get(<<"idLabels">>, Card, []),
                    true = (Labels =/= []) orelse (IdLabels =/= []),
                    ok
            end
    end.

update_labels_and_members(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id),
          application:get_env(trellang, label_id), application:get_env(trellang, member_id)} of
        {undefined, _, _, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined, _, _} -> {skip, "missing trellang.list_id in dev.config"};
        {_, _, undefined, _} -> {skip, "missing trellang.label_id in dev.config (optional test)"};
        {_, _, _, undefined} -> {skip, "missing trellang.member_id in dev.config (optional test)"};
        {_, _, {ok, LabelId0}, {ok, MemberId0}} ->
            LabelId = to_bin(LabelId0),
            MemberId = to_bin(MemberId0),
            {ok, Card0} = trello:create_card(#{name => <<"CT labels/members test">>}),
            CardId = maps:get(<<"id">>, Card0),
            {ok, _U1} = trello:update_card(CardId, #{idLabels => [LabelId]}),
            {ok, _U2} = trello:update_card(CardId, #{idMembers => [MemberId]}),
            {ok, Card} = trello:get_card(CardId),
            IdLabels = maps:get(<<"idLabels">>, Card),
            true = lists:member(LabelId, IdLabels),
            IdMembers = maps:get(<<"idMembers">>, Card),
            true = lists:member(MemberId, IdMembers),
            ok
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).

wait_card_with_labels(CardId, Attempts) when Attempts > 0 ->
    {ok, Card} = trello:get_card(CardId),
    Labels = maps:get(<<"labels">>, Card, []),
    IdLabels = maps:get(<<"idLabels">>, Card, []),
    case (Labels =/= []) orelse (IdLabels =/= []) of
        true -> Card;
        false -> timer:sleep(300), wait_card_with_labels(CardId, Attempts - 1)
    end.

wait_card_with_labels(CardId, 0) ->
    {ok, Card} = trello:get_card(CardId),
    Card.

find_any_label_id() ->
    Colors = [<<"green">>, <<"blue">>, <<"yellow">>, <<"red">>],
    find_first_ok(Colors).

find_first_ok([C | Rest]) ->
    case trello:get_label_id_by_color(C) of
        {ok, LabelId} -> {ok, LabelId};
        {error, not_found} -> find_first_ok(Rest);
        {error, _} -> find_first_ok(Rest)
    end;
find_first_ok([]) -> {error, not_found}.

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


