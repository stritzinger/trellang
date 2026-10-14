-module(trello_test_fixture).
-moduledoc false.
-behaviour(gen_server).

-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         start/2, create_card/3, start_board_server/4, create_server_card/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% The suite owns this process; individual testcase processes may be killed by CT.
%% Config carries its handle, never credentials or a process-dictionary registry.
init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(inets),
    case application:ensure_all_started(ssl) of
        {ok, SSLStarted} ->
            Apps = Started ++ SSLStarted,
            try
                case application:load(trellang) of
                    ok -> ok;
                    {error, {already_loaded, trellang}} -> ok
                end,
                Options = #{board_id => application:get_env(trellang, board_id, undefined),
                            cleanup => application:get_env(trellang, test_cleanup, false)},
                start([{fixture_apps, Apps} | Config], Options)
            catch Class:Reason:Stack ->
                stop_apps(Apps),
                erlang:raise(Class, Reason, Stack)
            end;
        {error, Reason} ->
            stop_apps(Started),
            error({fixture_start_failed, Reason})
    end.

end_per_suite(Config) ->
    Owner = proplists:get_value(fixture_owner, Config),
    Result = gen_server:call(Owner, cleanup_all, infinity),
    ok = stop_owner(Owner),
    stop_apps(proplists:get_value(fixture_apps, Config, [])),
    case Result of
        ok -> ok;
        {error, Reason} -> ct:fail(Reason)
    end.

init_per_testcase(_Case, Config) ->
    Owner = proplists:get_value(fixture_owner, Config),
    Ref = gen_server:call(Owner, new_case),
    [{fixture_case, Ref} | Config].

end_per_testcase(_Case, Config) ->
    case call(Config, cleanup) of
        ok -> Config;
        {error, Reason} -> {fail, Reason}
    end.

%% API injection is only for the offline fixture regression suite.
start(Config, Options) ->
    {ok, Owner} = gen_server:start(?MODULE, Options, []),
    [{fixture_owner, Owner} | Config].

create_card(Config, ListId, Fields) ->
    call(Config, {create_card, direct, to_bin(ListId), Fields}).

start_board_server(Config, Credentials, BoardId, Options) ->
    call(Config, {start_server, Credentials, to_bin(BoardId), Options}).

create_server_card(Config, Server, ListId, Fields) ->
    call(Config, {create_card, {server, Server}, to_bin(ListId), Fields}).

call(Config, Request) ->
    Owner = proplists:get_value(fixture_owner, Config),
    Ref = proplists:get_value(fixture_case, Config),
    gen_server:call(Owner, {Ref, Request}, infinity).

init(#{board_id := BoardId, cleanup := Cleanup} = Options) ->
    process_flag(trap_exit, true),
    {ok, #{board_id => to_bin(BoardId), cleanup => Cleanup,
           api => maps:get(api, Options, live_api()), cases => #{}}}.

handle_call(new_case, _From, #{cases := Cases} = State) ->
    Ref = make_ref(),
    {reply, Ref, State#{cases := Cases#{Ref => #{cards => [], servers => []}}}};
handle_call({Ref, {start_server, Credentials, BoardId, Options}}, _From,
            #{board_id := BoardId, api := API, cases := Cases} = State) ->
    #{servers := Servers} = Resources = maps:get(Ref, Cases),
    case invoke(maps:get(start_server, API), [Credentials, BoardId, Options]) of
        {ok, Pid} = Result ->
            link(Pid),
            {reply, Result, State#{cases := Cases#{Ref := Resources#{servers := [Pid | Servers]}}}};
        Error -> {reply, Error, State}
    end;
handle_call({_Ref, {start_server, _Credentials, _BoardId, _Options}}, _From, State) ->
    {reply, {error, board_outside_test_scope}, State};
handle_call({Ref, {create_card, Via, ListId, Fields}}, _From,
            #{api := API, cases := Cases} = State) ->
    Resources = maps:get(Ref, Cases),
    case validate_target(Via, ListId, Resources, State) of
        ok ->
            Name = owned_name(maps:get(name, Fields)),
            Pending = #{list_id => ListId, name => Name},
            Result = create(Via, ListId, Fields#{name := Name}, API),
            Card = case Result of
                {ok, #{<<"id">> := Id}} -> Pending#{id => Id};
                _ -> Pending
            end,
            #{cards := Cards} = Resources,
            %% Register before replying, including ambiguous remote failures.
            Updated = Resources#{cards := [Card | Cards]},
            {reply, Result, State#{cases := Cases#{Ref := Updated}}};
        Error -> {reply, Error, State}
    end;
handle_call({Ref, cleanup}, _From, #{cases := Cases} = State) ->
    {Result, Remaining} = cleanup(maps:get(Ref, Cases), State),
    {reply, Result, State#{cases := Cases#{Ref := Remaining}}};
handle_call(cleanup_all, _From, #{cases := Cases} = State) ->
    Results = [cleanup(Resources, State) || Resources <- maps:values(Cases)],
    Failed = lists:any(fun({Result, _}) -> Result =/= ok end, Results),
    Result = case Failed of
        false -> ok;
        true -> {error, fixture_cleanup_failed}
    end,
    {reply, Result, State#{cases := #{}}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% Server failures must not destroy the resource registry.
    {noreply, State};
handle_info(_Info, State) ->
    logger:debug("Unexpected message to test fixture"),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

validate_target({server, Server}, ListId, #{servers := Servers}, State) ->
    case lists:member(Server, Servers) of
        true -> validate_list(ListId, State);
        false -> {error, server_not_owned_by_test}
    end;
validate_target(direct, ListId, _Resources, State) ->
    validate_list(ListId, State).

validate_list(_ListId, #{board_id := undefined}) ->
    {error, missing_test_board};
validate_list(ListId, #{board_id := BoardId, api := #{list_lists := ListLists}}) ->
    case invoke(ListLists, [BoardId]) of
        {ok, Lists} when is_list(Lists) ->
            case lists:any(fun(#{<<"id">> := Id}) -> Id =:= ListId end, Lists) of
                true -> ok;
                false -> {error, list_outside_test_board}
            end;
        _ -> {error, cannot_validate_test_list}
    end.

create(direct, ListId, Fields, #{create_card := Create}) ->
    invoke(Create, [ListId, Fields]);
create({server, Server}, ListId, Fields, #{create_server_card := Create}) ->
    invoke(Create, [Server, ListId, Fields]).

cleanup(#{servers := Servers, cards := Cards}, State) ->
    FailedServers = [Pid || Pid <- Servers, stop_server(Pid) =/= ok],
    FailedCards = [Card || Card <- Cards, cleanup_card(Card, State) =/= ok],
    Result = case {FailedServers, FailedCards} of
        {[], []} -> ok;
        _ -> {error, fixture_cleanup_failed}
    end,
    {Result, #{servers => FailedServers, cards => FailedCards}}.

cleanup_card(_Card, #{cleanup := false}) -> ok;
cleanup_card(#{id := Id}, State) -> archive_owned_card(Id, State);
cleanup_card(#{list_id := ListId, name := Name}, #{api := API} = State) ->
    %% A server can create a card, then crash while refreshing before returning its id.
    %% Resolve only the cryptographically unique name recorded for this request.
    case validate_list(ListId, State) of
        ok ->
            case invoke(maps:get(list_cards, API), [ListId]) of
                {ok, Cards} ->
                    IDs = [Id || #{<<"id">> := Id, <<"name">> := N} <- Cards, N =:= Name],
                    Results = [archive_owned_card(Id, State) || Id <- IDs],
                    case lists:all(fun(Result) -> Result =:= ok end, Results) of
                        true -> ok;
                        false -> {error, archive_failed}
                    end;
                _ -> {error, resolve_created_card_failed}
            end;
        Error -> Error
    end.

archive_owned_card(Id, #{board_id := BoardId, api := API}) ->
    case invoke(maps:get(get_card, API), [Id]) of
        {ok, #{<<"idBoard">> := BoardId, <<"closed">> := true}} -> ok;
        {ok, #{<<"idBoard">> := BoardId}} ->
            case invoke(maps:get(archive_card, API), [Id]) of
                {ok, _} -> ok;
                _ -> {error, archive_failed}
            end;
        _ -> {error, card_outside_test_board_or_unavailable}
    end.

stop_server(Pid) ->
    Monitor = monitor(process, Pid),
    Result = try gen_server:stop(Pid, normal, 5000) of
        ok -> ok
    catch
        exit:noproc -> ok;
        exit:_ -> exit(Pid, kill), {error, server_stop_failed}
    end,
    receive
        {'DOWN', Monitor, process, Pid, _} -> Result
    after 5000 ->
        demonitor(Monitor, [flush]),
        {error, server_stop_timeout}
    end.

%% Return only bounded, credential-free errors from fixture infrastructure.
invoke(Function, Arguments) ->
    try apply(Function, Arguments) of
        {ok, _} = Result -> Result;
        {error, _} -> {error, fixture_api_failed}
    catch
        _:_ -> {error, fixture_api_exception}
    end.

owned_name(Name) ->
    Suffix = binary:encode_hex(crypto:strong_rand_bytes(16)),
    <<(to_bin(Name))/binary, " [trellang-test:", Suffix/binary, "]">>.

stop_owner(Owner) ->
    Monitor = monitor(process, Owner),
    gen_server:cast(Owner, stop),
    receive
        {'DOWN', Monitor, process, Owner, normal} -> ok
    after 5000 ->
        demonitor(Monitor, [flush]),
        error(fixture_stop_timeout)
    end.

stop_apps(Apps) ->
    lists:foreach(fun(App) -> application:stop(App) end, lists:reverse(Apps)).

to_bin(undefined) -> undefined;
to_bin(Binary) when is_binary(Binary) -> Binary;
to_bin(List) when is_list(List) -> unicode:characters_to_binary(List).

live_api() ->
    #{list_lists => fun trello:list_lists/1,
      create_card => fun trello:create_card/2,
      list_cards => fun(ListId) ->
          trello:list_cards_for_list(ListId, #{fields => <<"id,name,idBoard">>, limit => 1000})
      end,
      get_card => fun trello:get_card/1,
      archive_card => fun(Id) -> trello:update_card(Id, #{closed => true}) end,
      start_server => fun trello_board_server:start_link/3,
      create_server_card => fun trello_board_server:create_card/3}.
