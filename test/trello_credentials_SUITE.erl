-module(trello_credentials_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([me_with_explicit_credentials/1, me_with_invalid_credentials/1, me_parity_with_env/1, get_card_parity/1]).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [me_with_explicit_credentials, me_with_invalid_credentials, me_parity_with_env, get_card_parity].

init_per_suite(Config) ->
    trello_test_fixture:init_per_suite(Config).

end_per_suite(Config) ->
    trello_test_fixture:end_per_suite(Config).

init_per_testcase(Case, Config) ->
    trello_test_fixture:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    trello_test_fixture:end_per_testcase(Case, Config).

me_with_explicit_credentials(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined} -> {skip, "missing trellang.trello_token in dev.config"};
        {{ok, Key0}, {ok, Tok0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Me} = trello:me(Creds),
            _ = maps:get(<<"id">>, Me),
            _ = maps:get(<<"username">>, Me),
            ok
    end.

me_with_invalid_credentials(_Config) ->
    %% Intentionally invalid token
    case application:get_env(trellang, trello_key) of
        undefined -> {skip, "missing trellang.trello_key in dev.config"};
        {ok, Key0} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => <<"INVALID_TOKEN">>},
            case trello:me(Creds) of
                {error, {http_error, 401, _Body}} -> ok;
                {ok, _} -> ct:fail(unexpected_success_with_invalid_token);
                {error, Other} -> ct:fail({unexpected_error, Other})
            end
    end.

me_parity_with_env(_Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined} -> {skip, "missing trellang.trello_token in dev.config"};
        {{ok, Key0}, {ok, Tok0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Me1} = trello:me(),
            {ok, Me2} = trello:me(Creds),
            Id1 = maps:get(<<"id">>, Me1),
            Id2 = maps:get(<<"id">>, Me2),
            Id1 = Id2,
            ok
    end.

get_card_parity(Config) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token), application:get_env(trellang, list_id)} of
        {undefined, _, _} -> {skip, "missing trellang.trello_key in dev.config"};
        {_, undefined, _} -> {skip, "missing trellang.trello_token in dev.config"};
        {_, _, undefined} -> {skip, "missing trellang.list_id in dev.config"};
        {{ok, Key0}, {ok, Tok0}, {ok, ListId0}} ->
            Creds = #{trello_key => to_bin(Key0), trello_token => to_bin(Tok0)},
            {ok, Card0} = trello_test_fixture:create_card(Config, ListId0, #{name => <<"parity">>}),
            CardId = maps:get(<<"id">>, Card0),
            {ok, C1} = trello:get_card(CardId),
            {ok, C2} = trello:get_card(Creds, CardId),
            IdA = maps:get(<<"id">>, C1),
            IdB = maps:get(<<"id">>, C2),
            IdA = IdB,
            ok
    end.


to_bin(B) when is_binary(B) -> B;
 to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
 to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).
