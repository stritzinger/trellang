-module(trello_board_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([start_and_get_empty_state/1, refresh_populates_state/1, create_and_update_card_via_server/1]).

suite() -> [{timetrap, {seconds, 120}}].
all() -> [start_and_get_empty_state, refresh_populates_state, create_and_update_card_via_server].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

start_and_get_empty_state(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token), application:get_env(trellang, board_id)} of
        {undefined, _, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined, _} -> {skip, "missing trellang.trello_token in dev.config"};
        {_, _, undefined} -> {skip, "missing trellang.board_id in dev.config"};
        {{ok, Key0}, {ok, Tok0}, {ok, BoardId0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Pid} = trello_board_server:start_link(Creds, BoardId0, #{initial_refresh => false}),
            State = trello_board_server:get_state(Pid),
            BoardId = maps:get(board_id, State),
            true = is_binary(BoardId),
            [] = maps:get(lists, State),
            [] = maps:get(cards, State),
            [] = maps:get(checklists, State),
            ok = trello_board_server:stop(Pid),
            ok
    end.

refresh_populates_state(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token), application:get_env(trellang, board_id)} of
        {undefined, _, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined, _} -> {skip, "missing trellang.trello_token in dev.config"};
        {_, _, undefined} -> {skip, "missing trellang.board_id in dev.config"};
        {{ok, Key0}, {ok, Tok0}, {ok, BoardId0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Pid} = trello_board_server:start_link(Creds, BoardId0, #{initial_refresh => false}),
            ok = trello_board_server:refresh(Pid),
            State = trello_board_server:get_state(Pid),
            _ = maps:get(lists, State),
            _ = maps:get(cards, State),
            _ = maps:get(checklists, State),
            ok = trello_board_server:stop(Pid),
            ok
    end.

create_and_update_card_via_server(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token), application:get_env(trellang, board_id), application:get_env(trellang, list_id)} of
        {undefined, _, _, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined, _, _} -> {skip, "missing trellang.trello_token in dev.config"};
        {_, _, undefined, _} -> {skip, "missing trellang.board_id in dev.config"};
        {_, _, _, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {{ok, Key0}, {ok, Tok0}, {ok, BoardId0}, {ok, ListId0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Pid} = trello_board_server:start_link(Creds, BoardId0, #{initial_refresh => false}),
            {ok, Card} = trello_board_server:create_card(Pid, ListId0, #{name => <<"mirror-card">>}),
            CardId = maps:get(<<"id">>, Card),
            ok = trello_board_server:refresh(Pid),
            State1 = trello_board_server:get_state(Pid),
            Cards = maps:get(cards, State1),
            true = lists:any(fun(#{<<"id">> := Id}) -> Id =:= CardId; (_) -> false end, Cards),
            {ok, _} = trello_board_server:update_card(Pid, CardId, #{name => <<"mirror-card-upd">>}),
            ok = trello_board_server:refresh(Pid),
            State2 = trello_board_server:get_state(Pid),
            Cards2 = maps:get(cards, State2),
            true = lists:any(fun(#{<<"id">> := Id, <<"name">> := N}) -> Id =:= CardId andalso N =:= <<"mirror-card-upd">>; (_) -> false end, Cards2),
            ok = trello_board_server:stop(Pid),
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

to_bin(B) when is_binary(B) -> B;
 to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
 to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).
