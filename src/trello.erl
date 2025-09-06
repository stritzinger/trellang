-module(trello).

-export([
    me/0
]).

-define(BASE_URL, "https://api.trello.com/1").

me() ->
    do_get("/members/me", []).

%% Internal helpers

do_get(Path, QueryKVs) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {error, missing_trello_key};
        {_, undefined} -> {error, missing_trello_token};
        {{ok, Key0}, {ok, Token0}} ->
            Key = to_bin(Key0),
            Token = to_bin(Token0),
            Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
            Url = build_url(Path, Query),
            http_get_json(Url)
    end.

build_url(Path, QueryKVs) ->
    Qs = uri_string:compose_query(QueryKVs),
    unicode:characters_to_list([?BASE_URL, Path, $?, Qs]).

http_get_json(Url) ->
    case http_get_json_sni(Url, "api.trello.com") of
        {error, {failed_connect, _}} ->
            %% Fallback SNI host if handshake/hostname check failed
            case http_get_json_sni(Url, "trello.com") of
                {error, _}=E2 -> E2;
                Ok -> Ok
            end;
        Other -> Other
    end.

http_get_json_sni(Url, SNIHost) ->
    SSLOpts = [
        {server_name_indication, SNIHost},
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()}
    ],
    HTTPOpts = [{ssl, SSLOpts}],
    ReqOpts = [{body_format, binary}],
    case httpc:request(get, {Url, []}, HTTPOpts, ReqOpts) of
        {ok, {{_Vsn, 200, _}, _Headers, Body}} ->
            {ok, json:decode(Body)};
        {ok, {{_Vsn, Code, _}, _Headers, Body}} ->
            {error, {http_error, Code, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I).


