-module(trello).

-export([
    me/0,
    get_card/1,
    create_card/2,
    update_card/2,
    get_label_id_by_color/2,
    add_label/2,
    add_member/2,
    list_board_usernames/1,
    get_member_id_by_username/2
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

do_post(Path, QueryKVs) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {error, missing_trello_key};
        {_, undefined} -> {error, missing_trello_token};
        {{ok, Key0}, {ok, Token0}} ->
            Key = to_bin(Key0),
            Token = to_bin(Token0),
            Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
            Url = build_url(Path, Query),
            http_post_json(Url)
    end.

get_card(CardId0) ->
    CardId = to_bin(CardId0),
    Fields = <<"id,name,desc,due,pos,idBoard">>,
    do_get(["/cards/", CardId], [
        {<<"fields">>, Fields},
        {<<"labels">>, <<"all">>}
    ]).

create_card(ListId0, Options) when is_map(Options) ->
    ListId = to_bin(ListId0),
    NameBin = to_bin(maps:get(name, Options, <<>>)),
    Query = [
        {<<"idList">>, ListId},
        {<<"name">>, NameBin}
    ],
    do_post("/cards", Query).

update_card(CardId0, Updates) when is_map(Updates) ->
    CardId = to_bin(CardId0),
    Query0 = maps:fold(fun
        (name, V, Acc) -> [{<<"name">>, to_bin(V)} | Acc];
        (desc, V, Acc) -> [{<<"desc">>, to_bin(V)} | Acc];
        (due, V, Acc)  -> [{<<"due">>, to_bin(V)} | Acc];
        (pos, V, Acc)  -> [{<<"pos">>, to_bin(V)} | Acc];
        (idLabels, Vals, Acc) when is_list(Vals) ->
            %% Trello supports idLabels[]=A&idLabels[]=B for replace
            Pairs = [{<<"idLabels[]">>, to_bin(V)} || V <- Vals],
            Pairs ++ Acc;
        (idMembers, Vals, Acc) when is_list(Vals) ->
            Pairs = [{<<"idMembers[]">>, to_bin(V)} || V <- Vals],
            Pairs ++ Acc;
        (_K, _V, Acc) -> Acc
    end, [], Updates),
    Fields = <<"id,name,desc,due,pos,idBoard,idLabels,labels,idMembers">>,
    Query = [{<<"fields">>, Fields} | Query0],
    do_put(["/cards/", CardId], Query).

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

http_post_json(Url) ->
    case http_post_json_sni(Url, "api.trello.com") of
        {error, {failed_connect, _}} ->
            case http_post_json_sni(Url, "trello.com") of
                {error, _}=E2 -> E2;
                Ok -> Ok
            end;
        Other -> Other
    end.

http_post_json_sni(Url, SNIHost) ->
    SSLOpts = [
        {server_name_indication, SNIHost},
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()}
    ],
    HTTPOpts = [{ssl, SSLOpts}],
    Headers = [],
    Body = <<>>,
    ReqOpts = [{body_format, binary}],
    case httpc:request(post, {Url, Headers, "application/x-www-form-urlencoded", Body}, HTTPOpts, ReqOpts) of
        {ok, {{_Vsn, Code, _}, _Headers, RespBody}} when Code =:= 200; Code =:= 201 ->
            {ok, json:decode(RespBody)};
        {ok, {{_Vsn, Code2, _}, _Headers2, RespBody2}} ->
            {error, {http_error, Code2, RespBody2}};
        {error, Reason} -> {error, Reason}
    end.

do_put(Path, QueryKVs) ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {undefined, _} -> {error, missing_trello_key};
        {_, undefined} -> {error, missing_trello_token};
        {{ok, Key0}, {ok, Token0}} ->
            Key = to_bin(Key0),
            Token = to_bin(Token0),
            Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
            Url = build_url(Path, Query),
            http_put_json(Url)
    end.

http_put_json(Url) ->
    case http_put_json_sni(Url, "api.trello.com") of
        {error, {failed_connect, _}} ->
            case http_put_json_sni(Url, "trello.com") of
                {error, _}=E2 -> E2;
                Ok -> Ok
            end;
        Other -> Other
    end.

http_put_json_sni(Url, SNIHost) ->
    SSLOpts = [
        {server_name_indication, SNIHost},
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()}
    ],
    HTTPOpts = [{ssl, SSLOpts}],
    Headers = [],
    Body = <<>>,
    ReqOpts = [{body_format, binary}],
    case httpc:request(put, {Url, Headers, "application/x-www-form-urlencoded", Body}, HTTPOpts, ReqOpts) of
        {ok, {{_Vsn, Code, _}, _Headers, RespBody}} when Code =:= 200; Code =:= 201 ->
            {ok, json:decode(RespBody)};
        {ok, {{_Vsn, Code2, _}, _Headers2, RespBody2}} ->
            {error, {http_error, Code2, RespBody2}};
        {error, Reason} -> {error, Reason}
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I).


%% Utility: resolve a label id on the configured board by standard color or name
get_label_id_by_color(BoardId0, Color0) ->
    BoardId = to_bin(BoardId0),
    Color = normalize_color(Color0),
    case do_get(["/boards/", BoardId, "/labels"], [{<<"limit">>, <<"1000">>}]) of
        {ok, Labels} when is_list(Labels) ->
            find_label_id_by_color_or_name(Labels, Color);
        Other -> Other
    end.

find_label_id_by_color_or_name(Labels, Color) ->
    %% Prefer exact color match, fallback to name match (lowercased)
    ColorPairs = [{normalize_color(maps:get(<<"color">>, L, <<>>)), L} || L <- Labels],
    case lists:keyfind(Color, 1, ColorPairs) of
        {_, L} -> {ok, maps:get(<<"id">>, L)};
        false ->
            NamePairs = [{normalize_color(maps:get(<<"name">>, L, <<>>)), L} || L <- Labels],
            case lists:keyfind(Color, 1, NamePairs) of
                {_, L2} -> {ok, maps:get(<<"id">>, L2)};
                false -> {error, not_found}
            end
    end.

normalize_color(V) ->
    unicode:characters_to_binary(
      string:to_lower(unicode:characters_to_list(to_bin(V)))).

%% Add a label to a card by id
add_label(CardId0, LabelId0) ->
    CardId = to_bin(CardId0),
    LabelId = to_bin(LabelId0),
    do_post(["/cards/", CardId, "/idLabels"], [{<<"value">>, LabelId}]).

%% Add a member to a card by id
add_member(CardId0, MemberId0) ->
    CardId = to_bin(CardId0),
    MemberId = to_bin(MemberId0),
    do_post(["/cards/", CardId, "/idMembers"], [{<<"value">>, MemberId}]).

%% List usernames on the configured board
list_board_usernames(BoardId0) ->
    BoardId = to_bin(BoardId0),
    case do_get(["/boards/", BoardId, "/members"], [{<<"fields">>, <<"id,username">>}]) of
        {ok, Members} when is_list(Members) ->
            {ok, [ maps:get(<<"username">>, M, <<>>) || M <- Members ]};
        Other -> Other
    end.

%% Resolve member id by username (case-insensitive)
get_member_id_by_username(BoardId0, User0) ->
    BoardId = to_bin(BoardId0),
    U = normalize_color(User0), %% reuse lowercasing helper
    case do_get(["/boards/", BoardId, "/members"], [{<<"fields">>, <<"id,username">>}]) of
        {ok, Members} when is_list(Members) ->
            Pairs = [{normalize_color(maps:get(<<"username">>, M, <<>>)), M} || M <- Members],
            case lists:keyfind(U, 1, Pairs) of
                {_, MFound} -> {ok, maps:get(<<"id">>, MFound)};
                false -> {error, not_found}
            end;
        Other -> Other
    end.


