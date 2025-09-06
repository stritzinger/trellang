-module(trello).

-moduledoc """
Trello REST client (board-agnostic)

This module provides a small, pragmatic client for Trello's REST API focused on
working with cards on existing boards. It is board-agnostic by design: only the
API key and token are read from the application environment (`trellang.trello_key`
and `trellang.trello_token`). All identifiers such as `board_id`, `list_id`,
`member_id`, and `label_id` are passed as function parameters.

Highlights
- Authenticate using key/token from application env
- Create/read/update cards (name, desc, due, pos)
- Add labels by standard color (green/blue/yellow/red) and by discovered id
- Add members by username (resolved to id), or by known id
- Read and set custom fields (text/date/checkbox)
- Discover lists and list cards; dump a board structure into an Erlang term

Configuration
- Keep a non-committed `dev.config` and run with `ERL_FLAGS="-config ./dev.config"`
- The module reads only: `trellang.trello_key`, `trellang.trello_token`
- Tests (outside this module) may read fixed ids like `board_id` and `list_id`

Examples
Create and update a card
```erlang
{ok, ListId} = application:get_env(trellang, list_id),
{ok, BoardId} = application:get_env(trellang, board_id),

{ok, Card} = trello:create_card(ListId, #{name => <<"Hello Trello">>}),
CardId = maps:get(<<"id">>, Card),

{ok, _} = trello:set_desc(CardId, <<"Created from Erlang">>),
{ok, _} = trello:set_due(CardId, <<"2031-01-02T03:04:05.000Z">>),
{ok, _} = trello:add_label_by_color(BoardId, CardId, <<"green">>),
{ok, Card2} = trello:get_card(CardId).
```

Custom fields (text/date/checkbox)
```erlang
{ok, Fields} = trello:list_custom_fields(BoardId),
TextId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"text">>])),
{ok, Card} = trello:create_card(ListId, #{name => <<"CF demo">>}),
CardId = maps:get(<<"id">>, Card),
{ok, _} = trello:set_custom_field_text(CardId, TextId, <<"hello-cf">>),
{ok, CardCF} = trello:get_card_with_custom_fields(CardId).
```

Lists and board dump
```erlang
{ok, Lists} = trello:list_lists(BoardId),
{ok, Dump} = trello:dump_board(BoardId, #{}).
%% Dump = #{ board => #{<<"id">> := _, <<"name">> := _}
%%        , lists => [ #{<<"id">> := _, <<"name">> := _, <<"cards">> := [#{<<"id">> := _} | _]}
%%                    | _ ] }.
```

Documentation
This module uses OTP 27 documentation attributes (`-moduledoc`/`-doc`). See the
official guidance for details on style and metadata.
""".

-export([
    me/0,
    get_card/1,
    create_card/2,
    update_card/2,
    get_label_id_by_color/2,
    add_label/2,
    add_member/2,
    %% Ergonomic helpers
    set_name/2,
    set_desc/2,
    set_due/2,
    clear_due/1,
    set_pos/2,
    add_label_by_color/3,
    add_member_by_username/3,
    list_board_usernames/1,
    get_member_id_by_username/2,
    list_custom_fields/1,
    set_custom_field_text/3,
    get_card_with_custom_fields/1,
    get_card_labels/1,
    get_card_members/1,
    set_custom_field_date/3,
    set_custom_field_checkbox/3,
    list_lists/1,
    list_cards_for_list/2,
    dump_board/2
]).

-define(BASE_URL, "https://api.trello.com/1").

-doc """
Fetch the authenticated member ("/members/me").

Returns the member map on success.

Example:
```erlang
{ok, Me} = trello:me(),
Username = maps:get(<<"username">>, Me).
```
""".
me() ->
    do_get("/members/me", []).

%% Internal helpers

do_get(Path, QueryKVs) ->
    {ok, Key, Token} = get_credentials(),
    Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
    Url = build_url(Path, Query),
    http_get_json(Url).

do_post(Path, QueryKVs) ->
    {ok, Key, Token} = get_credentials(),
    Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
    Url = build_url(Path, Query),
    http_post_json(Url).

-doc """
Get a card by id.

Includes basic fields plus labels and members.

Example:
```erlang
{ok, Card} = trello:get_card(<<"CARD_ID">>),
Name = maps:get(<<"name">>, Card).
```
""".
get_card(CardId0) ->
    CardId = to_bin(CardId0),
    Fields = <<"id,name,desc,due,pos,idBoard">>,
    do_get(["/cards/", CardId], [
        {<<"fields">>, Fields},
        {<<"labels">>, <<"all">>},
        {<<"members">>, <<"true">>}
    ]).

-doc """
Create a card on a list.

Required: `ListId`. Options typically include `name`.

Example:
```erlang
{ok, Card} = trello:create_card(<<"LIST_ID">>, #{name => <<"New card">>}),
CardId = maps:get(<<"id">>, Card).
```
""".
create_card(ListId0, Options) when is_map(Options) ->
    ListId = to_bin(ListId0),
    NameBin = to_bin(maps:get(name, Options, <<>>)),
    Query = [
        {<<"idList">>, ListId},
        {<<"name">>, NameBin}
    ],
    do_post("/cards", Query).

-doc """
Update a card by id.

Supports name, desc, due, pos, idLabels[], idMembers[].

Example:
```erlang
{ok, _} = trello:update_card(<<"CARD_ID">>, #{desc => <<"Updated">>, pos => <<"top">>}).
```
""".
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
    do_get_with_retry(Url, HTTPOpts, ReqOpts, 5, 0).

do_get_with_retry(Url, HTTPOpts, ReqOpts, Attempts, Backoff) when Attempts > 0 ->
    case httpc:request(get, {Url, []}, HTTPOpts, ReqOpts) of
        {ok, {{_Vsn, 200, _}, _Headers, Body}} -> {ok, json:decode(Body)};
        {ok, {{_Vsn, Code, _}, _Headers, _Body}} when Code =:= 429; Code >= 500 ->
            timer:sleep(Backoff + jitter()),
            do_get_with_retry(Url, HTTPOpts, ReqOpts, Attempts - 1, next_backoff(Backoff));
        {ok, {{_Vsn, Code2, _}, _H2, Body2}} -> {error, {http_error, Code2, Body2}};
        {error, Reason} -> {error, Reason}
    end;
do_get_with_retry(_Url, _HTTPOpts, _ReqOpts, 0, _Backoff) ->
    {error, retry_exhausted}.

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
    do_write_with_retry(post, Url, Headers, "application/x-www-form-urlencoded", Body, HTTPOpts, ReqOpts, 5, 0).

do_put(Path, QueryKVs) ->
    {ok, Key, Token} = get_credentials(),
    Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
    Url = build_url(Path, Query),
    http_put_json(Url).

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
    do_write_with_retry(put, Url, Headers, "application/x-www-form-urlencoded", Body, HTTPOpts, ReqOpts, 5, 0).

do_put_body(Path, QueryKVs, MapBody) ->
    {ok, Key, Token} = get_credentials(),
    Query = [{<<"key">>, Key}, {<<"token">>, Token} | QueryKVs],
    Url = build_url(Path, Query),
    SSLOpts = [
        {server_name_indication, "api.trello.com"},
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()}
    ],
    HTTPOpts = [{ssl, SSLOpts}],
    BodyBin = json:encode(MapBody),
    Headers = [{"Content-Type", "application/json"}],
    ReqOpts = [{body_format, binary}],
    do_write_with_retry(put, Url, Headers, "application/json", BodyBin, HTTPOpts, ReqOpts, 5, 0).

do_write_with_retry(Method, Url, Headers, ContentType, Body, HTTPOpts, ReqOpts, Attempts, Backoff) when Attempts > 0 ->
    case httpc:request(Method, {Url, Headers, ContentType, Body}, HTTPOpts, ReqOpts) of
        {ok, {{_Vsn, Code, _}, _Headers, RespBody}} when Code =:= 200; Code =:= 201 ->
            {ok, json:decode(RespBody)};
        {ok, {{_Vsn, Code, _}, _Headers, _RespBody}} when Code =:= 429; Code >= 500 ->
            timer:sleep(Backoff + jitter()),
            do_write_with_retry(Method, Url, Headers, ContentType, Body, HTTPOpts, ReqOpts, Attempts - 1, next_backoff(Backoff));
        {ok, {{_Vsn, Code2, _}, _Headers2, RespBody2}} ->
            {error, {http_error, Code2, RespBody2}};
        {error, Reason} -> {error, Reason}
    end;
do_write_with_retry(_M, _U, _H, _C, _B, _O, _R, 0, _Backoff) ->
    {error, retry_exhausted}.

jitter() ->
    rand:uniform(100).

next_backoff(0) -> 200;
next_backoff(B) -> min(B * 2, 5000).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I).

%% Credentials helper: fetch key/token from application env, return binaries
get_credentials() ->
    case {application:get_env(trellang, trello_key), application:get_env(trellang, trello_token)} of
        {{ok, Key0}, {ok, Token0}} -> {ok, to_bin(Key0), to_bin(Token0)};
        {undefined, _} -> {error, missing_trello_key};
        {_, undefined} -> {error, missing_trello_token};
        {OtherKey, OtherTok} -> {error, {invalid_credentials_env, OtherKey, OtherTok}}
    end.


%% Utility: resolve a label id on the configured board by standard color or name
-doc """
Resolve a label id on a board by standard color or label name.

Example:
```erlang
{ok, LabelId} = trello:get_label_id_by_color(<<"BOARD_ID">>, <<"green">>).
```
""".
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
-doc """
Add a label to a card by label id.

Example:
```erlang
{ok, _} = trello:add_label(<<"CARD_ID">>, <<"LABEL_ID">>).
```
""".
add_label(CardId0, LabelId0) ->
    CardId = to_bin(CardId0),
    LabelId = to_bin(LabelId0),
    do_post(["/cards/", CardId, "/idLabels"], [{<<"value">>, LabelId}]).

%% Add a member to a card by id
-doc """
Add a member to a card by member id.

Example:
```erlang
{ok, _} = trello:add_member(<<"CARD_ID">>, <<"MEMBER_ID">>).
```
""".
add_member(CardId0, MemberId0) ->
    CardId = to_bin(CardId0),
    MemberId = to_bin(MemberId0),
    do_post(["/cards/", CardId, "/idMembers"], [{<<"value">>, MemberId}]).

%% List usernames on the configured board
-doc """
List usernames for all members on a board.
Returns `{ok, [UsernameBin,...]}`.

Example:
```erlang
{ok, Names} = trello:list_board_usernames(<<"BOARD_ID">>).
```
""".
list_board_usernames(BoardId0) ->
    BoardId = to_bin(BoardId0),
    case do_get(["/boards/", BoardId, "/members"], [{<<"fields">>, <<"id,username">>}]) of
        {ok, Members} when is_list(Members) ->
            {ok, [ maps:get(<<"username">>, M, <<>>) || M <- Members ]};
        Other -> Other
    end.

%% Resolve member id by username (case-insensitive)
-doc """
Resolve a member id by username (case-insensitive) on a board.

Example:
```erlang
{ok, MemberId} = trello:get_member_id_by_username(<<"BOARD_ID">>, <<"someuser">>).
```
""".
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

%% Custom Fields
-doc """
List custom field definitions for a board.

Example:
```erlang
{ok, Fields} = trello:list_custom_fields(<<"BOARD_ID">>).
```
""".
list_custom_fields(BoardId0) ->
    BoardId = to_bin(BoardId0),
    case do_get(["/boards/", BoardId, "/customFields"], []) of
        {ok, Fields} when is_list(Fields) -> {ok, Fields};
        Other -> Other
    end.

-doc """
Set a text custom field value on a card.

Example:
```erlang
{ok, _} = trello:set_custom_field_text(<<"CARD_ID">>, <<"FIELD_ID">>, <<"hello">>).
```
""".
set_custom_field_text(CardId0, FieldId0, Text0) ->
    CardId = to_bin(CardId0),
    FieldId = to_bin(FieldId0),
    Text = to_bin(Text0),
    Body = #{
        <<"value">> => #{ <<"text">> => Text }
    },
    do_put_body(["/card/", CardId, "/customField/", FieldId, "/item"], [], Body).

-doc """
Set a date custom field value (ISO-8601) on a card.

Example:
```erlang
{ok, _} = trello:set_custom_field_date(<<"CARD_ID">>, <<"FIELD_ID">>, <<"2031-12-24T12:34:56.000Z">>).
```
""".
set_custom_field_date(CardId0, FieldId0, DateIso8601) ->
    CardId = to_bin(CardId0),
    FieldId = to_bin(FieldId0),
    Date = to_bin(DateIso8601),
    Body = #{
        <<"value">> => #{ <<"date">> => Date }
    },
    do_put_body(["/card/", CardId, "/customField/", FieldId, "/item"], [], Body).

-doc """
Set a checkbox custom field value on a card (true/false).

Example:
```erlang
{ok, _} = trello:set_custom_field_checkbox(<<"CARD_ID">>, <<"FIELD_ID">>, true).
```
""".
set_custom_field_checkbox(CardId0, FieldId0, Bool) ->
    CardId = to_bin(CardId0),
    FieldId = to_bin(FieldId0),
    Checked = case Bool of true -> <<"true">>; false -> <<"false">>; _ -> <<"false">> end,
    Body = #{
        <<"value">> => #{ <<"checked">> => Checked }
    },
    do_put_body(["/card/", CardId, "/customField/", FieldId, "/item"], [], Body).

-doc """
Get a card with `customFieldItems=true` for reading custom field values.

Example:
```erlang
{ok, Card} = trello:get_card_with_custom_fields(<<"CARD_ID">>),
Items = maps:get(<<"customFieldItems">>, Card, []).
```
""".
get_card_with_custom_fields(CardId0) ->
    CardId = to_bin(CardId0),
    do_get(["/cards/", CardId], [
        {<<"customFieldItems">>, <<"true">>},
        {<<"fields">>, <<"id">>}
    ]).

-doc """
List label objects on a card.

Example:
```erlang
{ok, Labels} = trello:get_card_labels(<<"CARD_ID">>).
```
""".
get_card_labels(CardId0) ->
    CardId = to_bin(CardId0),
    do_get(["/cards/", CardId, "/labels"], [
        {<<"fields">>, <<"id,color,name">>}
    ]).

-doc """
List member objects on a card.

Example:
```erlang
{ok, Members} = trello:get_card_members(<<"CARD_ID">>).
```
""".
get_card_members(CardId0) ->
    CardId = to_bin(CardId0),
    do_get(["/cards/", CardId, "/members"], [
        {<<"fields">>, <<"id,username">>}
    ]).

%% Lists and Cards
-doc """
List lists on a board (id, name, closed, pos).

Example:
```erlang
{ok, Lists} = trello:list_lists(<<"BOARD_ID">>).
```
""".
list_lists(BoardId0) ->
    BoardId = to_bin(BoardId0),
    do_get(["/boards/", BoardId, "/lists"], [
        {<<"fields">>, <<"id,name,closed,pos">>}
    ]).

-doc """
List cards on a list with selectable `fields` and `limit`.

Example:
```erlang
{ok, Cards} = trello:list_cards_for_list(<<"LIST_ID">>, #{fields => <<"id,name">>, limit => 100}).
```
""".
list_cards_for_list(ListId0, Opts) ->
    ListId = to_bin(ListId0),
    Query = build_query_from_opts(Opts, [fields, limit]),
    do_get(["/lists/", ListId, "/cards"], Query).

-doc """
Dump a board into an Erlang term with lists and their cards.
Returns `{ok, #{board => BoardMap, lists => [ListMap#{<<"cards">> := [CardMap,...]}]}}`.

Example:
```erlang
{ok, Dump} = trello:dump_board(<<"BOARD_ID">>, #{}),
Lists = maps:get(lists, Dump).
```
""".
dump_board(BoardId0, _Opts) ->
    BoardId = to_bin(BoardId0),
    {ok, Board} = do_get(["/boards/", BoardId], [{<<"fields">>, <<"id,name">>}]),
    {ok, Lists} = list_lists(BoardId),
    CardsPerList = lists:map(
      fun(L) ->
          Lid = maps:get(<<"id">>, L),
          {ok, Cards} = list_cards_for_list(Lid, #{fields => <<"id,name,idList">>, limit => 1000}),
          L#{ <<"cards">> => Cards }
      end, Lists),
    {ok, #{ board => Board, lists => CardsPerList }}.

build_query_from_opts(Opts, Keys) ->
    maps:fold(fun(K, V, Acc) ->
        case lists:member(K, Keys) of
            true -> [{atom_to_binary(K, utf8), to_bin(V)} | Acc];
            false -> Acc
        end
    end, [], Opts).

%% Ergonomic helpers

-doc """
Set card name.

Example:
```erlang
{ok, _} = trello:set_name(<<"CARD_ID">>, <<"New Name">>).
```
""".
set_name(CardId, Name) ->
    update_card(CardId, #{name => Name}).

-doc """
Set card description.

Example:
```erlang
{ok, _} = trello:set_desc(<<"CARD_ID">>, <<"Some details">>).
```
""".
set_desc(CardId, Desc) ->
    update_card(CardId, #{desc => Desc}).

-doc """
Set card due date (ISO-8601) or <<"null">>.

Example:
```erlang
{ok, _} = trello:set_due(<<"CARD_ID">>, <<"2031-01-02T03:04:05.000Z">>).
```
""".
set_due(CardId, DateIso8601OrNull) ->
    update_card(CardId, #{due => DateIso8601OrNull}).

-doc """
Clear card due date.

Example:
```erlang
{ok, _} = trello:clear_due(<<"CARD_ID">>).
```
""".
clear_due(CardId) ->
    update_card(CardId, #{due => <<"null">>}).

-doc """
Set card position (number or <<"top">>/<<"bottom">>).

Example:
```erlang
{ok, _} = trello:set_pos(<<"CARD_ID">>, <<"top">>).
```
""".
set_pos(CardId, Pos) ->
    %% Pos can be a number or one of <<"top">>, <<"bottom">>
    update_card(CardId, #{pos => Pos}).

-doc """
Add a label to a card by standard color on the given board.

Example:
```erlang
{ok, _} = trello:add_label_by_color(<<"BOARD_ID">>, <<"CARD_ID">>, <<"red">>).
```
""".
add_label_by_color(BoardId, CardId, Color) ->
    case get_label_id_by_color(BoardId, Color) of
        {ok, LabelId} -> add_label(CardId, LabelId);
        Error -> Error
    end.

-doc """
Add a member to a card by username on the given board.

Example:
```erlang
{ok, _} = trello:add_member_by_username(<<"BOARD_ID">>, <<"CARD_ID">>, <<"someuser">>).
```
""".
add_member_by_username(BoardId, CardId, Username) ->
    case get_member_id_by_username(BoardId, Username) of
        {ok, MemberId} -> add_member(CardId, MemberId);
        Error -> Error
    end.


