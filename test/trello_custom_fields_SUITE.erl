-module(trello_custom_fields_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([list_custom_fields/1, set_date_and_checkbox_fields/1]).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [list_custom_fields, set_date_and_checkbox_fields].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

list_custom_fields(_Config) ->
    case application:get_env(trellang, board_id) of
        undefined -> {skip, "missing trellang.board_id in dev.config"};
        {ok, BoardId} ->
            {ok, Fields} = trello:list_custom_fields(BoardId),
            true = is_list(Fields),
            ok
    end.

set_date_and_checkbox_fields(_Config) ->
    case {application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {{ok, BoardId}, {ok, ListId}} ->
            {ok, Fields} = trello:list_custom_fields(BoardId),
            DateFields = [ F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"date">> ],
            CheckboxFields = [ F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"checkbox">> ],
            case {DateFields, CheckboxFields} of
                {[], _} -> {skip, "no date custom field on board"};
                {_, []} -> {skip, "no checkbox custom field on board"};
                {[DF | _], [CF | _]} ->
                    DateFieldId = maps:get(<<"id">>, DF),
                    CheckboxFieldId = maps:get(<<"id">>, CF),
                    {ok, Card0} = trello:create_card(ListId, #{name => <<"CT custom date/checkbox">>}),
                    CardId = maps:get(<<"id">>, Card0),
                    Date = <<"2031-12-24T12:34:56.000Z">>,
                    {ok, _} = trello:set_custom_field_date(CardId, DateFieldId, Date),
                    {ok, _} = trello:set_custom_field_checkbox(CardId, CheckboxFieldId, true),
                    {ok, Card} = trello:get_card_with_custom_fields(CardId),
                    Items = maps:get(<<"customFieldItems">>, Card, []),
                    true = lists:any(fun(#{<<"idCustomField">> := Id, <<"value">> := #{<<"date">> := D}}) -> Id =:= DateFieldId andalso D =:= Date; (_) -> false end, Items),
                    true = lists:any(fun(#{<<"idCustomField">> := Id, <<"value">> := #{<<"checked">> := <<"true">>}}) -> Id =:= CheckboxFieldId; (_) -> false end, Items),
                    ok
            end
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


