-module(trello_checklists_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [list_empty_on_new_card].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Config.

end_per_testcase(_TC, Config) ->
    %% Cleanup is handled by per-test logic if enabled via trellang.test_cleanup
    Config.

list_empty_on_new_card(_Config) ->
    {ok, ListId} = application:get_env(trellang, list_id),
    {ok, #{<<"id">> := CardId}} = trello:create_card(ListId, #{name => <<"checklists-baseline">>}),
    try
        {ok, Checklists} = trello:list_checklists(CardId),
        true = is_list(Checklists),
        [] = Checklists
    after
        maybe_cleanup_card(CardId)
    end.

maybe_cleanup_card(CardId) ->
    case application:get_env(trellang, test_cleanup) of
        {ok, true} ->
            _ = trello:update_card(CardId, #{closed => true}),
            ok;
        _ -> ok
    end.


