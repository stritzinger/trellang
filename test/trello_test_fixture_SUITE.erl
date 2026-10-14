-module(trello_test_fixture_SUITE).
-moduledoc false.

-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, init_per_testcase/2, end_per_testcase/2]).
-export([rejects_unscoped_resources/1, cleanup_isolates_testcases/1,
         failed_caller_keeps_resources/1, ambiguous_create_is_recovered/1,
         cleanup_failure_is_reported/1, moved_card_is_not_archived/1,
         server_crash_keeps_resources/1, cleanup_disabled_still_stops_server/1]).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    [rejects_unscoped_resources, cleanup_isolates_testcases,
     failed_caller_keeps_resources, ambiguous_create_is_recovered,
     cleanup_failure_is_reported, moved_card_is_not_archived,
     server_crash_keeps_resources, cleanup_disabled_still_stops_server].

init_per_testcase(Case, Config) ->
    Table = ets:new(cards, [public, set]),
    API = api(Table, Case),
    Suite = trello_test_fixture:start([{cards, Table} | Config],
                #{board_id => <<"board">>, cleanup => Case =/= cleanup_disabled_still_stops_server,
                  api => API}),
    trello_test_fixture:init_per_testcase(Case, Suite).

end_per_testcase(_Case, Config) ->
    try trello_test_fixture:end_per_suite(Config)
    after ets:delete(proplists:get_value(cards, Config))
    end.

rejects_unscoped_resources(Config) ->
    {error, list_outside_test_board} =
        trello_test_fixture:create_card(Config, <<"foreign-list">>, #{name => <<"reject">>}),
    {error, board_outside_test_scope} =
        trello_test_fixture:start_board_server(Config, credentials(), <<"foreign-board">>, #{}),
    {error, server_not_owned_by_test} =
        trello_test_fixture:create_server_card(Config, self(), <<"list">>, #{name => <<"reject">>}),
    [] = ets:tab2list(proplists:get_value(cards, Config)),
    ok.

cleanup_isolates_testcases(Config) ->
    Table = proplists:get_value(cards, Config),
    Existing = card(<<"existing">>, <<"same human name">>),
    ets:insert(Table, {<<"existing">>, Existing}),
    {ok, #{<<"id">> := First}} = create(Config),
    OtherConfig = trello_test_fixture:init_per_testcase(other_case, Config),
    {ok, #{<<"id">> := Second}} = create(OtherConfig),
    Config = trello_test_fixture:end_per_testcase(first_case, Config),
    true = closed(Table, First),
    false = closed(Table, Second),
    false = closed(Table, <<"existing">>),
    OtherConfig = trello_test_fixture:end_per_testcase(other_case, OtherConfig),
    true = closed(Table, Second),
    false = closed(Table, <<"existing">>),
    %% Repeated teardown remains safe.
    Config = trello_test_fixture:end_per_testcase(first_case, Config),
    ok.

failed_caller_keeps_resources(Config) ->
    {Pid, Monitor} = spawn_monitor(fun() -> create(Config) end),
    {Owner, Id} = receive
        {creation_started, FixtureOwner, CardId} -> {FixtureOwner, CardId}
    after 1000 -> ct:fail(create_timeout)
    end,
    %% Kill the caller after the remote side effect, before its reply arrives.
    exit(Pid, kill),
    receive {'DOWN', Monitor, process, Pid, killed} -> ok
    after 1000 -> ct:fail(caller_exit_timeout)
    end,
    Owner ! finish_creation,
    Config = trello_test_fixture:end_per_testcase(failed_case, Config),
    true = closed(proplists:get_value(cards, Config), Id),
    ok.

ambiguous_create_is_recovered(Config) ->
    Table = proplists:get_value(cards, Config),
    ets:insert(Table, {<<"existing">>, card(<<"existing">>, <<"same human name">>)}),
    {ok, Server} = start_server(Config),
    {error, fixture_api_failed} = trello_test_fixture:create_server_card(
        Config, Server, <<"list">>, #{name => <<"same human name">>}),
    Config = trello_test_fixture:end_per_testcase(failed_create, Config),
    [Created] = [Card || {Id, Card} <- ets:tab2list(Table), Id =/= <<"existing">>],
    true = maps:get(<<"closed">>, Created),
    false = closed(Table, <<"existing">>),
    ok.

cleanup_failure_is_reported(Config) ->
    Table = proplists:get_value(cards, Config),
    {ok, #{<<"id">> := Id}} = create(Config),
    {fail, fixture_cleanup_failed} = trello_test_fixture:end_per_testcase(example, Config),
    false = closed(Table, Id),
    %% A subsequent teardown retries the owned card rather than dropping it.
    Config = trello_test_fixture:end_per_testcase(example, Config),
    true = closed(Table, Id),
    ok.

moved_card_is_not_archived(Config) ->
    Table = proplists:get_value(cards, Config),
    {ok, #{<<"id">> := Id} = Card} = create(Config),
    ets:insert(Table, {Id, Card#{<<"idBoard">> := <<"foreign-board">>}}),
    {fail, fixture_cleanup_failed} = trello_test_fixture:end_per_testcase(example, Config),
    false = closed(Table, Id),
    ets:insert(Table, {Id, Card}),
    Config = trello_test_fixture:end_per_testcase(example, Config),
    true = closed(Table, Id),
    ok.

server_crash_keeps_resources(Config) ->
    {ok, #{<<"id">> := Id}} = create(Config),
    {ok, Server} = start_server(Config),
    Monitor = monitor(process, Server),
    exit(Server, kill),
    receive {'DOWN', Monitor, process, Server, killed} -> ok
    after 1000 -> ct:fail(server_exit_timeout)
    end,
    Config = trello_test_fixture:end_per_testcase(failed_case, Config),
    true = closed(proplists:get_value(cards, Config), Id),
    ?assert(is_process_alive(proplists:get_value(fixture_owner, Config))).

cleanup_disabled_still_stops_server(Config) ->
    {ok, #{<<"id">> := Id}} = create(Config),
    {ok, Server} = start_server(Config),
    Config = trello_test_fixture:end_per_testcase(example, Config),
    false = is_process_alive(Server),
    false = closed(proplists:get_value(cards, Config), Id),
    ok.

create(Config) ->
    trello_test_fixture:create_card(Config, <<"list">>, #{name => <<"same human name">>}).

start_server(Config) ->
    trello_test_fixture:start_board_server(Config, credentials(), <<"board">>,
                                         #{initial_refresh => false}).

credentials() -> #{trello_key => <<"dummy-key">>, trello_token => <<"dummy-token">>}.

card(Id, Name) ->
    #{<<"id">> => Id, <<"name">> => Name, <<"idBoard">> => <<"board">>,
      <<"idList">> => <<"list">>, <<"closed">> => false}.

closed(Table, Id) ->
    [{Id, Card}] = ets:lookup(Table, Id),
    maps:get(<<"closed">>, Card).

api(Table, Case) ->
    Observer = self(),
    Create = fun(<<"list">>, #{name := Name}) ->
          Id = integer_to_binary(erlang:unique_integer([positive])),
          Card = card(Id, Name),
          ets:insert(Table, {Id, Card}),
          case Case of
              ambiguous_create_is_recovered -> {error, response_lost};
              failed_caller_keeps_resources ->
                  Observer ! {creation_started, self(), Id},
                  receive finish_creation -> {ok, Card}
                  after 1000 -> {error, test_reply_timeout}
                  end;
              _ -> {ok, Card}
          end
      end,
    #{list_lists => fun(<<"board">>) -> {ok, [#{<<"id">> => <<"list">>}]} end,
      create_card => Create,
      list_cards => fun(<<"list">>) -> {ok, [Card || {_, Card} <- ets:tab2list(Table)]} end,
      get_card => fun(Id) ->
          [{Id, Card}] = ets:lookup(Table, Id),
          {ok, Card}
      end,
      archive_card => fun(Id) ->
          [{Id, Card}] = ets:lookup(Table, Id),
          case {Case, maps:get(attempted, Card, false)} of
              {cleanup_failure_is_reported, false} ->
                  ets:insert(Table, {Id, Card#{attempted => true}}),
                  {error, temporary_failure};
              _ ->
                  Archived = Card#{<<"closed">> := true},
                  ets:insert(Table, {Id, Archived}),
                  {ok, Archived}
          end
      end,
      start_server => fun trello_board_server:start_link/3,
      create_server_card => fun(Server, ListId, Fields) ->
          Result = Create(ListId, Fields),
          exit(Server, kill),
          Result
      end}.
