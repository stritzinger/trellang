-module(trello_credentials_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([me_with_explicit_credentials/1, me_with_invalid_credentials/1, me_parity_with_env/1]).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [me_with_explicit_credentials, me_with_invalid_credentials, me_parity_with_env].

init_per_suite(Config) ->
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_loaded(trellang),
    Config.

end_per_suite(_Config) -> ok.

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
